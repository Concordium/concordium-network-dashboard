import { Elm } from "./elm/Main"
import io from 'socket.io-client'

const dashboardHost = window.location.host + '/frontends'

document.addEventListener("DOMContentLoaded", function() {
  let app = Elm.Main.init({
    flags: null
  })

  console.log(`Connecting to ${dashboardHost}`)
  var socket = io.connect(dashboardHost)

  socket.on('nodeInfo', function (nodeData) {
    console.log('js:nodeInfo received:', nodeData)
    app.ports.nodeInfo.send(nodeData)
  })

  // app.ports.hello.subscribe(message => socket.emit('toServer', message))
})


// Reload every 15 minutes or so - dumb way to keep our office screens on latest UI version
setTimeout(() => { window.location.reload() }, 15 * 60 * 1000);
