import { Elm } from "./elm/Main"
import io from 'socket.io-client'

document.addEventListener("DOMContentLoaded", function() {
  let app = Elm.Main.init({
    flags: null
  })

  console.log(`Connecting to ${window.location.host + '/frontends'}`)
  var socket = io.connect(window.location.host + '/frontends')

  socket.on('nodeInfo', function (nodeData) {
    app.ports.nodeInfo.send(nodeData)
  })

  // app.ports.hello.subscribe(message => socket.emit('toServer', message))
})