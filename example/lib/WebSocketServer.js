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



var lastMessage = null;
var lastId = null;

function handleConnection (input, output, verbose, ws) {

  const id = uuid();
  const requestUrl = 'ws://' + ws.upgradeReq.headers.host + ws.upgradeReq.url;
  const location = Location(requestUrl);

  if (verbose) console.log('connected:', id);

  function recieve (message) {
    if (verbose) console.log('receive:', message, '\n  from:', id);
    input.send(Message(id, location, message));
  };

  function handleCommand (command) {
    switch (command.type) {
      case 'Close':
        if (command.id === id) {
          if (verbose) console.log('close:', id);
          lastMessage = null;
          ws.close();
        };
        return;
      case 'Message':
        if (command.id === id) {
          try {
            if (verbose)  {
              if (lastMessage == command.message && lastId != id) {
                console.log('  to:', id);
              } else {
                lastMessage = command.message;
                lastId = id;
                console.log('send:', lastMessage, '\n  to:', id);
              }
            }
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
    input.send(Disconnection(id, location));
    ws.removeListener('message', recieve);
    ws.removeListener('close', close);
    ws.removeListener('error', close);
    if (verbose) console.log('disconnected:', id);
  }

  ws.on('message', recieve);
  ws.on('close', close);
  ws.on('error', close);

  output.subscribe(handleCommand);

  input.send(Connection(id, location));

};



function WebSocketServer (server, inputPort, outputPort, verbose) {
  const wss = new WSS({ server: server });
  wss.on('connection', handleConnection.bind(null, inputPort, outputPort, verbose));
  return wss;
};



exports = module.exports = WebSocketServer;
