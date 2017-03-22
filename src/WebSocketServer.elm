module WebSocketServer exposing
  ( Socket
  , Config
  , close
  , sendToOne
  , sendToMany
  , sendToOthers
  , eventDecoder
  )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

import Navigation exposing (Location)

type alias Socket = String

type alias Config msg =
  { onConnection : Socket -> Location -> msg
  , onDisconnection: Socket -> Location -> msg
  , onMessage: Socket -> Location -> Decoder msg
  }



-- COMMANDS

close : (Encode.Value -> a) -> Socket -> a
close outputPort = encodeClose >> outputPort

sendToOne : (Encode.Value -> a) -> Encode.Value -> Socket -> a
sendToOne outputPort = curry (encodeMessage >> outputPort)

sendToMany : (Encode.Value -> a) -> Encode.Value -> List Socket -> List a
sendToMany outputPort message sockets =
  List.map (sendToOne outputPort message) sockets

sendToOthers : (Encode.Value -> a) -> Encode.Value -> Socket -> List Socket -> List a
sendToOthers outputPort message socket sockets =
  let
    others = List.filter ((/=) socket) sockets
  in
    sendToMany outputPort message others



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

eventDecoder : Config msg -> Decoder msg
eventDecoder config =
  Decode.field "type" Decode.string
    |> Decode.andThen (msgTypeDecoder config)

msgTypeDecoder : Config msg -> String -> Decoder msg
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
  decode Location
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
