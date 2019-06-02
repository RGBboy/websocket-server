port module Server exposing (Model, Msg(..), decodeMsg, inputPort, main, outputPort, subscriptions, update)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Platform exposing (Program)
import WebSocketServer as WSS exposing (Socket, sendToMany, sendToOne)


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( [], Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port inputPort : (Decode.Value -> msg) -> Sub msg


port outputPort : Encode.Value -> Cmd msg



-- MODEL


type alias Model =
    List WSS.Socket



-- UPDATE


type Msg
    = Connection WSS.Socket
    | Disconnection WSS.Socket
    | Message String
    | Noop


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case Debug.log "Msg" msg of
        Connection socket ->
            ( socket :: model
            , Cmd.none
            )

        Disconnection socket ->
            ( List.filter ((/=) socket) model
            , Cmd.none
            )

        Message message ->
            ( model
            , WSS.sendToMany outputPort message model
                |> Cmd.batch
            )

        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


decodeMsg : Decode.Value -> Msg
decodeMsg value =
    let
        decoder =
            WSS.eventDecoder
                { onConnection = \socket _ -> Connection socket
                , onDisconnection = \socket _ -> Disconnection socket
                , onMessage = \_ _ message -> Message message
                }
    in
    Decode.decodeValue decoder value
        |> Result.withDefault Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    inputPort decodeMsg
