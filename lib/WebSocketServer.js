var url = require('url'),
    uuid = require('uuid').v4,
    WSS = require('ws').Server;



function Connection (id, url) {
  return {
    type: 'Connection',
    id: id,
    url: url
  };
};



function Disconnection (id, url) {
  return {
    type: 'Disconnection',
    id: id,
    url: url
  };
};



function Message (id, url, message) {
  return {
    type: 'Message',
    id: id,
    url: url,
    message: message
  };
};



function handleConnection (input, output, ws, request) {

  const id = uuid();
  // below we use the http protocol as Elm only supports HTTP or HTTPS for urls
  const requestUrl = 'http://' + request.headers.host + request.url;

  // message comes through as a buffer
  function receive (message) {
    input.send(Message(id, requestUrl, message.toString()));
  };

  function handleCommand (command) {
    switch (command.type) {
      case 'Close':
        if (command.id === id) {
          ws.close();
        };
        return;
      case 'Message':
        if (command.id === id) {
          try {
            ws.send(command.message);
          } catch (_) {}
        };
        return;
      default:
        return;
    };
  };

  function close (err) {
    output.unsubscribe(handleCommand);
    input.send(Disconnection(id, requestUrl));
    ws.removeListener('message', receive);
    ws.removeListener('close', close);
    ws.removeListener('error', close);
  }

  ws.on('message', receive);
  ws.on('close', close);
  ws.on('error', close);

  output.subscribe(handleCommand);

  input.send(Connection(id, requestUrl));

};



function WebSocketServer (server, inputPort, outputPort) {
  const wss = new WSS({ server: server });
  wss.on('connection', handleConnection.bind(null, inputPort, outputPort));
  return wss;
};



exports = module.exports = WebSocketServer;
