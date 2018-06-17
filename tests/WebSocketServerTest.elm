module WebSocketServerTest exposing (..)

import Expect
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Navigation
import String
import Test exposing (Test, describe, test)
import WebSocketServer exposing (..)


location : Location
location =
    Navigation.Location "ws://localhost:8080/123" "localhost:8080" "localhost" "ws:" "ws://localhost:8080" "8080" "/123" "" "" "" ""


connectionJSON : String
connectionJSON =
    """
{
  "type": "Connection",
  "id": "abc",
  "location": {
    "protocol": "ws:",
    "hash": "",
    "search": "",
    "pathname": "/123",
    "port_": "8080",
    "hostname": "localhost",
    "host": "localhost:8080",
    "origin": "ws://localhost:8080",
    "href": "ws://localhost:8080/123",
    "username" : "",
    "password" : ""
  }
}
"""


disconnectionJSON : String
disconnectionJSON =
    """
{
  "type": "Disconnection",
  "id": "abc",
  "location": {
    "protocol": "ws:",
    "hash": "",
    "search": "",
    "pathname": "/123",
    "port_": "8080",
    "hostname": "localhost",
    "host": "localhost:8080",
    "origin": "ws://localhost:8080",
    "href": "ws://localhost:8080/123",
    "username" : "",
    "password" : ""
  }
}
"""


messageJSON : String
messageJSON =
    """
{
  "type": "Message",
  "id": "abc",
  "location": {
    "protocol": "ws:",
    "hash": "",
    "search": "",
    "pathname": "/123",
    "port_": "8080",
    "hostname": "localhost",
    "host": "localhost:8080",
    "origin": "ws://localhost:8080",
    "href": "ws://localhost:8080/123",
    "username" : "",
    "password" : ""
  },
  "message": "Test"
}
"""


type Msg
    = Connection Socket Location
    | Disconnection Socket Location
    | Message Socket Location String


config =
    { onConnection = Connection
    , onDisconnection = Disconnection
    , onMessage = Message
    }


expectDecode : Decoder a -> String -> (a -> Expect.Expectation) -> Expect.Expectation
expectDecode decoder input expectation =
    Result.withDefault (Expect.fail "Unable to decode")
        (Result.map expectation (Decode.decodeString decoder input))


tests : Test
tests =
    describe "WebSocketServer"
        [ describe ".eventDecoder"
            [ test "decodes Connection events" <|
                \() ->
                    expectDecode (eventDecoder config)
                        connectionJSON
                        (flip Expect.equal (Connection "abc" location))
            , test "decodes Disconnection events" <|
                \() ->
                    expectDecode (eventDecoder config)
                        disconnectionJSON
                        (flip Expect.equal (Disconnection "abc" location))
            , test "decodes Message events" <|
                \() ->
                    expectDecode (eventDecoder config)
                        messageJSON
                        (flip Expect.equal (Message "abc" location "Test"))
            ]
        , describe ".close"
            [ test "close" <|
                \() ->
                    let
                        actual =
                            Encode.encode 2 (close identity "a")

                        expected =
                            """{
  "type": "Close",
  "id": "a"
}"""
                    in
                    Expect.equal actual expected
            ]
        , describe ".sendToOne"
            [ test "sendToOne" <|
                \() ->
                    let
                        actual =
                            Encode.encode 2 (sendToOne identity "Test" "a")

                        expected =
                            """{
  "type": "Message",
  "id": "a",
  "message": "Test"
}"""
                    in
                    Expect.equal actual expected
            ]
        , describe ".sendToMany"
            [ test "sendToMany" <|
                \() ->
                    let
                        actual =
                            List.map (Encode.encode 2) (sendToMany identity "Test" [ "a", "b" ])

                        expected =
                            [ """{
  "type": "Message",
  "id": "a",
  "message": "Test"
}"""
                            , """{
  "type": "Message",
  "id": "b",
  "message": "Test"
}"""
                            ]
                    in
                    Expect.equalLists actual expected
            ]
        , describe ".sendToOthers"
            [ test "sendToOthers" <|
                \() ->
                    let
                        actual =
                            List.map (Encode.encode 2) (sendToOthers identity "Test" "a" [ "a", "b", "c" ])

                        expected =
                            [ """{
  "type": "Message",
  "id": "b",
  "message": "Test"
}"""
                            , """{
  "type": "Message",
  "id": "c",
  "message": "Test"
}"""
                            ]
                    in
                    Expect.equalLists actual expected
            ]
        ]
