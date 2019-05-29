import * as _ from 'lodash'
import * as express from 'express'
import * as http from 'http'
import * as socketio from 'socket.io'
import { staticsRouter } from './routes/statics-router'
import { staticsDevRouter } from './routes/statics-dev-router'
import * as config from './config'
import { getVersion } from '../shared/utils'
import * as auth from 'http-auth'

console.log(`The App version is ${getVersion()}`)

const app = express()

var nodesSummary = {}

// This endpoint serves the current set of all node summaries that have been seen
// and is updated on every node summary that comes in. We're using nginx in front
// of this endpoint to apply the microcaching approach:
// https://www.nginx.com/blog/benefits-of-microcaching-nginx/
// which means frontend clients will see what appears to be a static snapshot
// that updates every 1 second, even though this endpoint itself is dynamic
app.get('/data/nodesSummary', function(req, res){
  res.json(_.values(nodesSummary))
})


// Debug endpoint to allow resetting of nodes data cache to clear out old nodes
// This is a weak protection as there is no real risk to resetting this value
const basic = auth.basic({realm: "Protected"}, function(username, password, callback) {
  callback(username === "concordium" && password === "development");
})
app.get('/dev/reset', auth.connect(basic), function(req, res){
  nodesSummary = {}
  res.send('nodeSummaries reset')
})

app.use(config.IS_PRODUCTION ? staticsRouter() : staticsDevRouter())

const server = http.createServer(app)
var io = socketio(server)

const nodes = io.of('/nodes')
// const frontends = io.of('/frontends')

nodes.on('connection', socket => {
  console.log(`Connection from node ${socket.handshake.headers.host}`)
  socket.on('nodeInfo', node => {
    nodesSummary[node.nodeName] = node
    // frontends.emit('nodeInfo', node)
  })
})

// frontends.on('connection', socket => {
//   console.log(`Connection from frontend ${socket.handshake.headers.host}`)
// })

server.listen(config.SERVER_PORT, () => {
  console.log(`App listening on port ${config.SERVER_PORT}!`)
})
