import * as express from 'express';
import * as http from 'http';
import * as socketio from 'socket.io'
import { staticsRouter } from './routes/statics-router';
import { staticsDevRouter } from './routes/statics-dev-router';
import * as config from './config';
import { getVersion } from '../shared/utils';
import * as grpc from 'grpc';
import * as protoLoader from '@grpc/proto-loader';


console.log(`The App version is ${getVersion()}`);

const app = express();

app.use(config.IS_PRODUCTION ? staticsRouter() : staticsDevRouter());

const server = http.createServer(app);
var io = socketio(server);

var PROTO_PATH = __dirname + '/../../../proto/concordium_p2p_rpc.proto';

var packageDefinition = protoLoader.loadSync(
    PROTO_PATH,
    {keepCase: true,
     longs: String,
     enums: String,
     defaults: true,
     oneofs: true
    });
var protoDescriptor = grpc.loadPackageDefinition(packageDefinition);

var meta = new grpc.Metadata();
meta.add('authentication', 'rpcadmin');

// @TODO this needs to either "discover" nodes or know about them already.
var grpcService = new protoDescriptor.P2P('localhost:8890', grpc.credentials.createInsecure());

grpcService.peerUptime({}, meta, function(err, response) {
  if (err) {
    console.log(`Got error: ${err}`)
  } else {
    console.log('toClient', response);
  }
});

io.on('connection', function(socket) {
  socket.emit('toClient', 'Hello from server!');
  socket.on('toServer', function(message) {
    console.log(message);
  });

  socket.on('uptime', function(uptime) {
    grpcService.peerUptime({}, meta, function(err, response) {
      if (err) {
        console.log(`Got error: ${err}`)
      } else {
        socket.emit('toClient', response);
      }
    });

  });
});

server.listen(config.SERVER_PORT, () => {
  console.log(`App listening on port ${config.SERVER_PORT}!`);
});
