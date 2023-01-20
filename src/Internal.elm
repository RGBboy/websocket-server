module Internal exposing
  ( Socket
  , socket
  , socketToString
  , decodeSocket
  , encodeSocket
  )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

type Socket = Socket String

socket : String -> Socket
socket value = Socket value

socketToString : Socket -> String
socketToString (Socket value) = value

encodeSocket : Socket -> Encode.Value
encodeSocket = socketToString >> Encode.string

decodeSocket : Decoder Socket
decodeSocket =
  Decode.string
    |> Decode.map Socket