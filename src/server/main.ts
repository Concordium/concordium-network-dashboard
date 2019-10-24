import * as _ from 'lodash'
import * as express from 'express'
import * as http from 'http'
import { staticsRouter } from './routes/statics-router'
import { staticsDevRouter } from './routes/statics-dev-router'
import * as config from './config'
import { getVersion } from '../shared/utils'
import * as auth from 'http-auth'

console.log(`The App version is ${getVersion()}`)

const app = express()

app.use(config.IS_PRODUCTION ? staticsRouter() : staticsDevRouter())

const server = http.createServer(app)

server.listen(config.SERVER_PORT, () => {
  console.log(`App listening on port ${config.SERVER_PORT}!`)
  console.warn(`WARNING: This is the development-only backend.`)
  console.warn(`See here for the real ones:`)
  console.warn(`https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector.rs`)
  console.warn(`https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector_backend.rs`)
})
