port module Server exposing (..)

import Platform exposing (Program)
import Json.Decode as Decode
import Json.Encode as Encode
import WebSocketServer as WSS exposing (Socket, sendToOne, sendToMany)



main : Program () Model Msg
main =
  Platform.worker
    { init = always ([], Cmd.none)
    , update = update
    , subscriptions = subscriptions
    }

-- PORTS

port inputPort : (Decode.Value -> msg) -> Sub msg
port outputPort : Encode.Value -> Cmd msg

-- MODEL

type alias Model = List WSS.Socket

-- UPDATE

type Msg
  = Connection WSS.Socket
  | Disconnection WSS.Socket
  | Message Encode.Value
  | Noop

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case (Debug.log "Msg" message) of
    Connection socket ->
      ( socket :: model
      , Cmd.none
      )
    Disconnection socket ->
      ( List.filter ((/=) socket) model
      , Cmd.none
      )
    Message clientMessage ->
      let
        decodedMessage = Decode.decodeValue Decode.string clientMessage
          |> Debug.log "Message Value"
      in
        ( model
        , WSS.sendToMany outputPort clientMessage model
            |> Cmd.batch
        )
    Noop -> (model, Cmd.none)

-- SUBSCRIPTIONS

decodeMsg : Decode.Value -> Msg
decodeMsg value =
  let
    decoder = WSS.eventDecoder
      { onConnection = (\socket _ -> Connection socket)
      , onDisconnection = (\socket _ -> Disconnection socket)
      , onMessage = (\_ _ message -> Message message)
      }
  in
    Decode.decodeValue decoder value
      |> Result.withDefault Noop

subscriptions : Model -> Sub Msg
subscriptions model = inputPort decodeMsg
