var port = (process.env.PORT || 8080),
    http = require('http'),
    ecstatic = require('ecstatic'),
    server = http.createServer(
      ecstatic({ root: __dirname })
    ),
    WebSocketServer = require('../'),
    app = require('./server.js').Elm.Server.init(),
    wss = new WebSocketServer(
      server,
      app.ports.inputPort,
      app.ports.outputPort
    );

server.listen(port);

console.log(`Listening on :${port}`);
