import { Elm } from "./elm/Main"

document.addEventListener("DOMContentLoaded", function () {

  const layoutViewportSize = () => ({
    width: document.documentElement.clientWidth,
    height: document.documentElement.clientHeight
  })

  let app = Elm.Main.init({
    flags: layoutViewportSize()
  })

  // app.ports.hello.subscribe(message => socket.emit('toServer', message))
})

// Reload every 15 minutes or so - dumb way to keep our office screens on latest UI version
setTimeout(() => { window.location.reload() }, 15 * 60 * 1000);
