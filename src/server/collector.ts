// import * as express from 'express'
// import * as http from 'http'
// import * as socketio from 'socket.io'
// import { staticsRouter } from './routes/statics-router'
// import { staticsDevRouter } from './routes/statics-dev-router'
// import * as config from './config'
import { getVersion } from '../shared/utils'
import * as grpc from 'grpc'
import * as protoLoader from '@grpc/proto-loader'
import * as io from 'socket.io-client'
const interval = require('interval-promise')

console.log(`Collector version ${getVersion()}`)

var PROTO_PATH = __dirname + '/../../../proto/concordium_p2p_rpc.proto'

var packageDefinition = protoLoader.loadSync(
    PROTO_PATH,
    { keepCase: true,
      longs: String,
      enums: String,
      defaults: true,
      oneofs: true
    })
var protoDescriptor = grpc.loadPackageDefinition(packageDefinition)

var meta = new grpc.Metadata()
meta.add('authentication', 'rpcadmin')

// @TODO need to add config option to override, if local node is using non-standard port
var grpcService = new protoDescriptor.P2P('localhost:8890', grpc.credentials.createInsecure())

// @TODO need to add config option to override collector path
const dashboard = io('http://localhost:3000/nodes')


const getUptime = () => {
  return new Promise ((resolve, reject) => {
    grpcService.peerUptime({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(parseInt(response.value))
      }
    })
  })
}

const getBestBlockInfo = () => {
  return new Promise ((resolve, reject) => {
    grpcService.getBestBlockInfo({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(JSON.parse(response.best_block_info))
        // return JSON.parse(response.best_block_info)
      }
    })
  })
}

const main = async () => {

  const uptime = await getUptime()
  const bestBlockInfo = await getBestBlockInfo()

  // Try me once basics are working
  // const [ uptime, bestBlockInfo ] = Promise.all [ getUptime(), getBestBlockInfo() ]

  // { "best_block_info":
  //   "{\"globalState\":[[\"30\",\"0\"],[\"31\",\"0\"]]
  //   ,\"transactions\":[]
  //   ,\"blockParent\":\"2e10bf476aa901b8ce18777b9afd22ddf63ca747c36421fb2cae70e3dbd9ee65\"
  //   ,\"blockHash\":\"33e37742ead3708e74515cea59ca43c77815a4837c7a3ef495743b459d5e175e\"
  //   ,\"blockBaker\":1}"
  // }

  // @TODO make host configurable
  const nodeData = { host: 'node1', state: JSON.stringify(bestBlockInfo['globalState']), uptime: uptime }//, bestBlockHash: bestBlockInfo.blockHash }

  console.log('emitting: nodeInfo', nodeData)
  dashboard.emit('nodeInfo', nodeData)
};

// setInterval(() => {
//   console.log('rerunning')
//   main.then(console.log)
// }, 1000)

interval(async () => {
    await main()
}, 2000)



// @TODO do we remove this later, or is it unavoidable with node.js?
process.on('unhandledRejection', (err) => {
  console.error(err)
  process.exit(1)
})
