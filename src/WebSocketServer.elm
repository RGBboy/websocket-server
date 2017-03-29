module WebSocketServer exposing
  ( Socket
  , Location
  , close
  , sendToOne
  , sendToMany
  , sendToOthers
  , eventDecoder
  )

{-| Web socket server enables you to write the server part of your websocket
application in Elm.

# Web Socket Server
@docs Socket, Location, eventDecoder
# Commands
@docs sendToOne, sendToMany, sendToOthers, close
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Navigation



{-| A pointer to the socket in the node.js world. These are based on uuids and
are unique to each connection that is created.
-}
type alias Socket = String

{-| The same Location type as found in [`elm-lang/navigation`][navigation].

[navigation]: https://github.com/elm-lang/navigation
-}
type alias Location = Navigation.Location



-- COMMANDS

{-| Send a message to a particular socket. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

You would write something like this to create a cmd to send a message:

    sendToOne outputPort "Hello!" socketA
-}
sendToOne : (Encode.Value -> a) -> String -> Socket -> a
sendToOne outputPort = curry (encodeMessage >> outputPort)

{-| Send a message to a many sockets. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

You would write something like this to create a cmd to send messages:

    sendToMany outputPort "Hello!" [socketA, socketB]
      |> Cmd.batch
-}
sendToMany : (Encode.Value -> a) -> String -> List Socket -> List a
sendToMany outputPort message sockets =
  List.map (sendToOne outputPort message) sockets

{-| Send a message to a all sockets except one. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

You would write something like this to create a cmd to send messages:

    sendToOthers outputPort "Hello!" socketA [socketA, socketB, socketC]
      |> Cmd.batch
-}
sendToOthers : (Encode.Value -> a) -> String -> Socket -> List Socket -> List a
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
    , ("id", Encode.string socket)
    ]

encodeMessage : (String, Socket) -> Encode.Value
encodeMessage (message, socket) =
  Encode.object
    [ ("type", Encode.string "Message")
    , ("id", Encode.string socket)
    , ("message", Encode.string message)
    ]



-- DECODE

{-| Create a decoder to use with your input port. This allows you to hook into
the events that will be triggered over a sockets lifetime and respond to them
in your update function.

    onConnection : Socket -> Location -> msg

Triggered when a new connection is made. Can be used to get the new connection
and the Location that the connection was made to. This can be useful to
segregate connections into groups or associating a private id.

    onDisconnection : Socket -> Location -> msg

Triggered when a disconnection happens. Can be used to clean up the connection
from anywhere it has been saved in your application state.

    onMessage : Socket -> Location -> String -> msg

Triggered when a socket recieves a message.

**Note 1:** Almost everyone will want to use a URL parsing library like
[`evancz/url-parser`][parse] to turn a `Location` into something more useful.

[parse]: https://github.com/evancz/url-parser

-}
eventDecoder
  : { onConnection : Socket -> Location -> msg
    , onDisconnection: Socket -> Location -> msg
    , onMessage: Socket -> Location -> String -> msg
    }
  -> Decoder msg
eventDecoder config =
  Decode.field "type" Decode.string
    |> Decode.andThen (msgTypeDecoder config)

msgTypeDecoder
  : { onConnection : Socket -> Location -> msg
    , onDisconnection: Socket -> Location -> msg
    , onMessage: Socket -> Location -> String -> msg
    }
  -> String
  -> Decoder msg
msgTypeDecoder config kind =
  case kind of
    "Connection" ->
      decode config.onConnection
        |> required "id" Decode.string
        |> required "location" decodeLocation
    "Disconnection" ->
      decode config.onDisconnection
        |> required "id" Decode.string
        |> required "location" decodeLocation
    "Message" ->
      decode config.onMessage
        |> required "id" Decode.string
        |> required "location" decodeLocation
        |> required "message" Decode.string
    _ -> Decode.fail ("Could not decode msg of type " ++ kind)

decodeLocation : Decoder Location
decodeLocation =
  decode Navigation.Location
    |> required "href" Decode.string
    |> required "host" Decode.string
    |> required "hostname" Decode.string
    |> required "protocol" Decode.string
    |> required "origin" Decode.string
    |> required "port_" Decode.string
    |> required "pathname" Decode.string
    |> required "search" Decode.string
    |> required "hash" Decode.string
    |> required "username" Decode.string
    |> required "password" Decode.string
