module WebSocketServer exposing
  ( Socket
  , Location
  , close
  , sendToOne
  , sendToMany
  , sendToOthers
  , eventDecoder
  )

{-| Web sockets make it cheaper to talk to your servers.
Connecting to a server takes some time, so with web sockets, you make that
connection once and then keep using. The major benefits of this are:
  1. It faster to send messages. No need to do a bunch of work for every single
  message.
  2. The server can push messages to you. With normal HTTP you would have to
  keep *asking* for changes, but a web socket, the server can talk to you
  whenever it wants. This means there is less unnecessary network traffic.
The API here attempts to cover the typical usage scenarios, but if you need
many unique connections to the same endpoint, you need a different library.
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

{-| The same Location type as found in elm-lang/navigation
-}
type alias Location = Navigation.Location



-- COMMANDS

{-| Send a message to a particular socket. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

    You would write something like this to create a cmd to send a message:

    sendToOne outputPort (Encode.string "Hello!") socketA
-}
sendToOne : (Encode.Value -> a) -> Encode.Value -> Socket -> a
sendToOne outputPort = curry (encodeMessage >> outputPort)

{-| Send a message to a many sockets. Given you have an output port:

    port outputPort : Encode.Value -> Cmd msg

    You would write something like this to create a cmd to send messages:

    sendToMany outputPort (Encode.string "Hello!") [socketA, socketB]
      |> Cmd.batch
-}
sendToMany : (Encode.Value -> a) -> Encode.Value -> List Socket -> List a
sendToMany outputPort message sockets =
  List.map (sendToOne outputPort message) sockets

{-| Send a message to a all sockets except one. Given you have an output port:

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
    , ("id", Encode.string socket)
    ]

encodeMessage : (Encode.Value, Socket) -> Encode.Value
encodeMessage (message, socket) =
  Encode.object
    [ ("type", Encode.string "Message")
    , ("id", Encode.string socket)
    , ("data", message)
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

    onMessage : Socket -> Location -> Decoder msg

Triggered when a socket recieves a message.

**Note 1:** Almost everyone will want to use a URL parsing library like
[`evancz/url-parser`][parse] to turn a `Location` into something more useful.
[parse]: https://github.com/evancz/url-parser

-}
eventDecoder
  : { onConnection : Socket -> Location -> msg
    , onDisconnection: Socket -> Location -> msg
    , onMessage: Socket -> Location -> Decoder msg
    }
  -> Decoder msg
eventDecoder config =
  Decode.field "type" Decode.string
    |> Decode.andThen (msgTypeDecoder config)

msgTypeDecoder
  : { onConnection : Socket -> Location -> msg
    , onDisconnection: Socket -> Location -> msg
    , onMessage: Socket -> Location -> Decoder msg
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
        |> Decode.andThen (Decode.field "message")
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
