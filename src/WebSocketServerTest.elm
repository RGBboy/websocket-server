module WebSocketServerTest exposing (tests)

import Test exposing (Test, describe, test)
import Test.Runner.Node exposing (run, TestProgram)
import Expect
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url exposing (Url)

import WebSocketServer exposing (..)



url : Url
url = 
  { protocol = Url.Http
  , host = "localhost"
  , port_ = Just 8080
  , path = "/123"
  , query = Nothing
  , fragment = Nothing
  }

connectionJSON : String
connectionJSON = """
{
  "type": "Connection",
  "id": "abc",
  "url": "http://localhost:8080/123"
}
"""

disconnectionJSON : String
disconnectionJSON = """
{
  "type": "Disconnection",
  "id": "abc",
  "url": "http://localhost:8080/123"
}
"""

messageJSON : String
messageJSON = """
{
  "type": "Message",
  "id": "abc",
  "url": "http://localhost:8080/123",
  "message": "Test"
}
"""

type Msg
  = Connection Socket Url
  | Disconnection Socket Url
  | Message Socket Url String

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
          expectDecode (eventDecoder config) connectionJSON
            (Expect.equal (Connection "abc" url))
      , test "decodes Disconnection events" <|
        \() ->
          expectDecode (eventDecoder config) disconnectionJSON
            (Expect.equal (Disconnection "abc" url))
      , test "decodes Message events" <|
        \() ->
          expectDecode (eventDecoder config) messageJSON
            (Expect.equal (Message "abc" url "Test"))
      ]
    , describe ".close"
      [ test "close" <|
        \() ->
          let
            actual = Encode.encode 2 (close identity "a")
            expected = """{
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
            actual = Encode.encode 2 (sendToOne identity "Test" "a")
            expected = """{
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
            actual = List.map (Encode.encode 2) (sendToMany identity "Test" ["a", "b"])
            expected =
              ["""{
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
            actual = List.map (Encode.encode 2) (sendToOthers identity "Test" "a" ["a", "b", "c"])
            expected =
              ["""{
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
