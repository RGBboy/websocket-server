# WebSocket Server

Web Socket Server in Elm and Node.js

## Installation

To install the elm part of this library run:

```
elm package install RGBboy/websocket-server
```

To install the node part of this library run:

```
npm install elm-websocket-server
```

## Usage

See the example folder for a working version.

To get this working run:

```
npm install
npm run build:example
npm run start:example
```

Then go to localhost:8080 in multiple browser windows.

### Example Elm Server Program

```elm
port module Server exposing (..)

import Platform exposing (Program)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import WebSocketServer as WSS exposing (Socket, sendToOne, sendToMany)



main : Program Never Model Msg
main =
  Platform.program
    { init = ([], Cmd.none)
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
  | Message String
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
    Message message ->
      ( model
      , WSS.sendToMany outputPort message model
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
```

### Example Usage in Node.js

The node API for this is quite simple:

```javascript
var port = (process.env.PORT || 8080),
    server = require('http').createServer(),
    WebSocketServer = require('elm-websocket-server'),
    app = require('./my-elm-server.js').Main.worker(),
    wss = new WebSocketServer(
      server,
      app.ports.inputPort,
      app.ports.outputPort
    );

server.listen(port);

console.log(`Listening on :${port}`);
```

## Development

Run `npm install` then `elm package install`.

## Tests

Run `npm t`.
