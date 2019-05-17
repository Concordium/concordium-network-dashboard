import * as express from 'express'
import * as http from 'http'
import * as socketio from 'socket.io'
import { staticsRouter } from './routes/statics-router'
import { staticsDevRouter } from './routes/statics-dev-router'
import * as config from './config'
import { getVersion } from '../shared/utils'

import compression = require('compression')

console.log(`The App version is ${getVersion()}`)


const app = express()

app.use(compression())
app.use(config.IS_PRODUCTION ? staticsRouter() : staticsDevRouter())

const server = http.createServer(app)
var io = socketio(server)

const websocket = io.of('/websocket')

websocket.on('connection', socket => {
  // Unused currently, but in place for future.
  // socket.on('someEvent', msg => {
  //   socket.emit('something', someData)
  // })
})

server.listen(config.SERVER_PORT, () => {
  console.log(`App listening on port ${config.SERVER_PORT}!`)
})
