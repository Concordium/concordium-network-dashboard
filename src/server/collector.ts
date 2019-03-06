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
import * as _ from 'lodash'
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

const getPeerVersion = () => {
  return new Promise ((resolve, reject) => {
    grpcService.PeerVersion({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(response.value)
      }
    })
  })
}

const getPeerStats = () => {
  return new Promise ((resolve, reject) => {
    grpcService.PeerStats({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(response['peerstats'])
      }
    })
  })
}

const getPeerTotalSent = () => {
  return new Promise ((resolve, reject) => {
    grpcService.PeerTotalSent({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(parseInt(response.value))
      }
    })
  })
}

const getPeerTotalReceived = () => {
  return new Promise ((resolve, reject) => {
    grpcService.PeerTotalReceived({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(parseInt(response.value))
      }
    })
  })
}

const main = async () => {

  const uptime = await getUptime()
  const bestBlockInfo = await getBestBlockInfo()
  const client = await getPeerVersion()
  const peerStats = await getPeerStats()
  const packetsSent = await getPeerTotalSent()
  const packetsReceived = await getPeerTotalReceived()

  const peerCount = _.values(peerStats).length

  const nodeData = {
    host: 'node1', // @TODO make host configurable
    state: JSON.stringify(bestBlockInfo['globalState']),
    uptime: uptime,
    client: client,
    averagePing: _.sum(_.values(peerStats).map(n => parseInt(n['measured_latency']))) / peerCount,
    peersCount: peerCount,
    bestBlockHash: bestBlockInfo['blockHash'],
    packetsSent: packetsSent,
    packetsReceived: packetsReceived
  }

  console.log('emitting: nodeInfo', nodeData)
  dashboard.emit('nodeInfo', nodeData)
};


interval(async () => {
    await main()
}, 2000)


// @TODO do we remove this later, or is it unavoidable with node.js?
process.on('unhandledRejection', (err) => {
  console.error(err)
  process.exit(1)
})
