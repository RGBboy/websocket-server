port module Client exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Result



main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- PORTS

port receive : (Decode.Value -> msg) -> Sub msg
port send : Encode.Value -> Cmd msg



-- MODEL

type alias Model =
  { messages: List String
  , input: String
  }

init : () -> (Model, Cmd msg)
init _ =
  ( { messages = []
    , input = ""
    }
  , Cmd.none
  )



-- UPDATE

type Msg
  = InputMessage String
  | SubmitMessage
  | ServerMessage String
  | ServerError

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    InputMessage value ->
      ( { model | input = value }
      , Cmd.none
      )
    SubmitMessage ->
      ( { model | input = "" }
      , Encode.string model.input |> send
      )
    ServerMessage serverMessage ->
      ( { model
        | messages = serverMessage :: model.messages
        }
      , Cmd.none
      )
    ServerError ->
      ( model, Cmd.none )



-- SUBSCRIPTIONS

msgDecoder : Decoder Msg
msgDecoder =
  Decode.map ServerMessage Decode.string

decodeMsg : Decode.Value -> Msg
decodeMsg value =
  Decode.decodeValue msgDecoder value
    |> Result.withDefault ServerError

subscriptions : Model -> Sub Msg
subscriptions model =
  receive decodeMsg




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
