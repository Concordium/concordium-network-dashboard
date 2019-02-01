import { Elm } from "./elm/Main";
import io from 'socket.io-client';

document.addEventListener("DOMContentLoaded", function() {
  let app = Elm.Main.init({
    flags: null
  });

  var socket = io.connect(window.location.host);

  socket.on('toClient', function (message) {
    app.ports.reply.send(message);
  });

  app.ports.hello.subscribe(message => socket.emit('toServer', message));
});
