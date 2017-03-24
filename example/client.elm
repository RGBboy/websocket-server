module Client exposing (..)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import WebSocket



main : Program String Model Msg
main =
  H.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
  { messages: List String
  , input: String
  , server: String
  }

init : String -> (Model, Cmd msg)
init server =
  ( { messages = []
    , input = ""
    , server = server
    }
  , Cmd.none
  )



-- UPDATE

type Msg
  = InputMessage String
  | SubmitMessage
  | ServerMessage String
  | Noop

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    InputMessage value ->
      ( { model | input = value }
      , Cmd.none
      )
    SubmitMessage ->
      ( { model | input = "" }
      , WebSocket.send model.server (Encode.encode 2 (Encode.string model.input))
      )
    ServerMessage message ->
      ( { model
        | messages = message :: model.messages
        }
      , Cmd.none
      )
    Noop -> (model, Cmd.none)



-- SUBSCRIPTIONS

decoder : Decoder Msg
decoder =
  Decode.map ServerMessage Decode.string

decodeMessage : String -> Msg
decodeMessage message =
  Decode.decodeString decoder message
    |> Result.withDefault Noop

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen model.server decodeMessage



-- VIEW

onEnter : Msg -> H.Attribute Msg
onEnter message =
    E.on "keydown"
      (E.keyCode |> Decode.andThen (is13 message))

is13 : a -> Int -> Decoder a
is13 a code =
  if code == 13 then Decode.succeed a else Decode.fail "not the right key code"

messageView : String -> Html Msg
messageView message =
  H.li
    []
    [ H.text message ]

view : Model -> Html Msg
view model =
  H.div
    []
    [ H.ul [] (List.map messageView model.messages)
    , H.input
        [ A.type_ "text"
        , A.placeholder "Message..."
        , A.value model.input
        , E.onInput InputMessage
        , onEnter SubmitMessage
        ]
        []
    ]
