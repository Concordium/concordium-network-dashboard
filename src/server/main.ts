import * as express from 'express'
import * as http from 'http'
import staticsRouter from './routes/statics-router'
import staticsDevRouter from './routes/statics-dev-router'
import * as config from './config'
import { getVersion } from '../shared/utils'

console.log(`The App version is ${getVersion()}`)

const app = express()

app.use(config.isProduction ? staticsRouter() : staticsDevRouter())

const server = http.createServer(app)

server.listen(config.serverPort, () => {
  console.log(`App listening on port ${config.serverPort}!`)
  console.warn(`WARNING: This is the development-only backend.`)
  console.warn(`See here for the real ones:`)
  console.warn(`https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector.rs`)
  console.warn(`https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector_backend.rs`)
})
