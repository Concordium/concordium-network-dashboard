import * as express from 'express'
import * as http from 'http'
import * as socketio from 'socket.io'
import { staticsRouter } from './routes/statics-router'
import { staticsDevRouter } from './routes/statics-dev-router'
import * as config from './config'
import { getVersion } from '../shared/utils'

console.log(`The App version is ${getVersion()}`)

const app = express()

app.use(config.IS_PRODUCTION ? staticsRouter() : staticsDevRouter())

const server = http.createServer(app)
var io = socketio(server)

const nodes = io.of('/nodes')
const frontends = io.of('/frontends')

nodes.on('connection', socket => {
  console.log(`Connection from node ${socket.handshake.headers.host}`)
  socket.on('nodeInfo', node => {
    frontends.emit('nodeInfo', node)
  })
})

frontends.on('connection', socket => {
  console.log(`Connection from frontend ${socket.handshake.headers.host}`)
})

server.listen(config.SERVER_PORT, () => {
  console.log(`App listening on port ${config.SERVER_PORT}!`)
})
