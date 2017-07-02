var url = require('url'),
    uuid = require('uuid').v4,
    WSS = require('ws').Server;



function Location (nodeUrl) {
  parsedUrl = url.parse(nodeUrl);
  return {
    protocol : parsedUrl.protocol,
    hash : parsedUrl.hash || '',
    search : parsedUrl.search || '',
    pathname : parsedUrl.pathname,
    port_ : parsedUrl.port || '',
    hostname : parsedUrl.hostname,
    host : parsedUrl.host,
    origin : parsedUrl.protocol + '//' + parsedUrl.host,
    href: parsedUrl.href,
    username : '', // temp
    password : '' // temp
  };
}



function Connection (id, location) {
  return {
    type: 'Connection',
    id: id,
    location: location
  };
};



function Disconnection (id, location) {
  return {
    type: 'Disconnection',
    id: id,
    location: location
  };
};



function Message (id, location, message) {
  return {
    type: 'Message',
    id: id,
    location: location,
    message: message
  };
};



function handleConnection (input, output, ws) {

  const id = uuid();
  const requestUrl = 'ws://' + ws.upgradeReq.headers.host + ws.upgradeReq.url;
  const location = Location(requestUrl);

  function recieve (message) {
    input.send(Message(id, location, message));
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
          } catch (err) {
          }
        };
        return;
      default:
        return;
    };
  };

  function close (err) {
    output.unsubscribe(handleCommand);
    input.send(Disconnection(id, location));
    ws.removeListener('message', recieve);
    ws.removeListener('close', close);
    ws.removeListener('error', close);
  }

  ws.on('message', recieve);
  ws.on('close', close);
  ws.on('error', close);

  output.subscribe(handleCommand);

  input.send(Connection(id, location));

};



function WebSocketServer (server, inputPort, outputPort) {
  const wss = new WSS({ server: server });
  wss.on('connection', handleConnection.bind(null, inputPort, outputPort));
  return wss;
};



exports = module.exports = WebSocketServer;
