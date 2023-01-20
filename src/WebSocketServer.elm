module WebSocketServer exposing
  ( Socket
  , socketToString
  , close
  , sendToOne
  , sendToMany
  , sendToOthers
  , eventDecoder
  )

{-| Web socket server enables you to write the server part of your websocket
application in Elm.

# Web Socket Server
@docs Socket, socketToString, eventDecoder
# Commands
@docs sendToOne, sendToMany, sendToOthers, close
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url exposing (Url)
import Internal



{-| A pointer to the socket in the node.js world. These are based on uuids and
are unique to each connection that is created.
-}
type alias Socket = Internal.Socket

{-| Transform a Socket into a `String`. Useful if you want to use the value
as a key in a Dict.
-}
socketToString : Socket -> String
socketToString = Internal.socketToString



-- COMMANDS

{-| Send a message to a particular socket. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

You would write something like this to create a cmd to send a message:

    sendToOne outputPort (Encode.string "Hello!") socketA
-}
sendToOne : (Encode.Value -> a) -> Encode.Value -> Socket -> a
sendToOne outputPort message socket =
  encodeMessage message socket
    |> outputPort

{-| Send a message to a many sockets. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

You would write something like this to create a cmd to send messages:

    sendToMany outputPort (Encode.string "Hello!") [socketA, socketB]
      |> Cmd.batch
-}
sendToMany : (Encode.Value -> a) -> Encode.Value -> List Socket -> List a
sendToMany outputPort message sockets =
  List.map (sendToOne outputPort message) sockets

{-| Send a message to all sockets except one. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

You would write something like this to create a cmd to send messages:

    sendToOthers outputPort (Encode.string "Hello!") socketA [socketA, socketB, socketC]
      |> Cmd.batch
-}
sendToOthers : (Encode.Value -> a) -> Encode.Value -> Socket -> List Socket -> List a
sendToOthers outputPort message socket sockets =
  let
    others = List.filter ((/=) socket) sockets
  in
    sendToMany outputPort message others

{-| Close a socket connection. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

You would write something like this to create a cmd to close a socket:

    close outputPort socketA
-}
close : (Encode.Value -> a) -> Socket -> a
close outputPort = encodeClose >> outputPort



-- ENCODE

encodeClose : Socket -> Encode.Value
encodeClose socket =
  Encode.object
    [ ("type", Encode.string "Close")
    , ("id", Internal.encodeSocket socket)
    ]

encodeMessage : Encode.Value -> Socket -> Encode.Value
encodeMessage message socket =
  Encode.object
    [ ("type", Encode.string "Message")
    , ("id", Internal.encodeSocket socket)
    , ("message", message)
    ]



-- DECODE

{-| Create a decoder to use with your input port. This allows you to hook into
the events that will be triggered over a sockets lifetime and respond to them
in your update function.

    onConnection : Socket -> Url -> msg

Triggered when a new connection is made. Can be used to get the new connection
and the Url that the connection was made to. This can be useful to segregate 
connections into groups or associating a private id.

    onDisconnection : Socket -> Url -> msg

Triggered when a disconnection happens. Can be used to clean up the connection
from anywhere it has been saved in your application state.

    onMessage : Socket -> Url -> Decode.Value -> msg

Triggered when a socket recieves a message.

**Note:** Almost everyone will want to use a URL parsing library like
[`elm/url`][parse] to turn a `Url` into something more useful.

[parse]: https://github.com/elm/url

-}
eventDecoder
  : { onConnection : Socket -> Url -> msg
    , onDisconnection: Socket -> Url -> msg
    , onMessage: Socket -> Url -> Decode.Value -> msg
    }
  -> Decoder msg
eventDecoder config =
  Decode.field "type" Decode.string
    |> Decode.andThen (msgTypeDecoder config)

msgTypeDecoder
  : { onConnection : Socket -> Url -> msg
    , onDisconnection: Socket -> Url -> msg
    , onMessage: Socket -> Url -> Decode.Value -> msg
    }
  -> String
  -> Decoder msg
msgTypeDecoder config kind =
  case kind of
    "Connection" ->
      Decode.succeed config.onConnection
        |> required "id" Internal.decodeSocket
        |> required "url" decodeUrl
    "Disconnection" ->
      Decode.succeed config.onDisconnection
        |> required "id" Internal.decodeSocket
        |> required "url" decodeUrl
    "Message" ->
      Decode.succeed config.onMessage
        |> required "id" Internal.decodeSocket
        |> required "url" decodeUrl
        |> required "message" Decode.value
    _ -> Decode.fail ("Could not decode msg of type " ++ kind)

decodeUrl : Decoder Url
decodeUrl =
    Decode.string
      |> Decode.map Url.fromString
      |> Decode.map (Maybe.map Decode.succeed)
      |> Decode.map (Maybe.withDefault (Decode.fail "Could not decode url"))
      |> Decode.andThen identity