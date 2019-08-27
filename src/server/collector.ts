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

const program = require('commander')
const interval = require('interval-promise')

var nodeName = (process.env.COLLECTOR_NAME && (process.env.COLLECTOR_NAME != '')) ? process.env.COLLECTOR_NAME : 'unknown';
let defaultHost = (process.env.COLLECTOR_HOST && (process.env.COLLECTOR_HOST != '')) ? process.env.COLLECTOR_HOST : 'localhost:8890';
let defaultDashboard = (process.env.COLLECTOR_DASHBOARD && (process.env.COLLECTOR_DASHBOARD != '')) ? process.env.COLLECTOR_DASHBOARD : 'localhost:3000';
let defaultFrequency = (process.env.COLLECTOR_FREQUENCY && (process.env.COLLECTOR_FREQUENCY != '')) ? process.env.COLLECTOR_FREQUENCY : '2000';


program
  // @TODO restore this later to enforce required args for users
  // see note a few lines down too
  // .arguments('<node-name>')
  // .action(name => { nodeName = name })
  .option('-h, --host [default]', 'Specify node hostname [localhost:8890]', defaultHost)
  .option('-d, --dashboard [default]', 'Specify dashboard hostname, comma seperate for multiple [localhost:3000]', defaultDashboard)
  .version(getVersion(), '-v, --version')
  .parse(process.argv)

// @TODO restore this later to enforce required args for users
// we disabled it to just get going with k8s given it's easier to use ENV there
// if (!process.argv.slice(2).length) {
//   program.outputHelp()
//   process.exit()
// }

console.log(`Collector connecting to ${program.host} as ${nodeName}`)

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

const dashboards = _.map(program.dashboard.split(','), host => io('http://' + host + '/nodes'))

const getUptime = (grpcService) => {
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


const getConsensusStatus = (grpcService) => {
  return new Promise ((resolve, reject) => {
    grpcService.GetConsensusStatus({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(JSON.parse(response["json_value"]))
        // return JSON.parse(response.best_block_info)
      }
    })
  })
}


const getPeerVersion = (grpcService) => {
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

const getPeerStats = (grpcService) => {
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

const getPeerTotalSent = (grpcService) => {
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

const getPeerTotalReceived = (grpcService) => {
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

const getNodeInfo = (grpcService) => {
  return new Promise ((resolve, reject) => {
    grpcService.NodeInfo({}, meta, function(err, response) {
      if (err) {
        reject(err)
      } else {
        resolve(response)
      }
    })
  })
}

const main = async () => {

  var grpcService = new protoDescriptor.concordium.P2P(program.host, grpc.credentials.createInsecure())

  const uptime = await getUptime(grpcService)
  const consensusStatus = await getConsensusStatus(grpcService)
  const client = await getPeerVersion(grpcService)
  const peerStats = await getPeerStats(grpcService)
  const packetsSent = await getPeerTotalSent(grpcService)
  const packetsReceived = await getPeerTotalReceived(grpcService)
  const nodeInfo = await getNodeInfo(grpcService)
  const peerCount = _.values(peerStats).length

  const nodeData = {
    nodeName: nodeName,
    nodeId: nodeInfo['node_id']['value'],
    peerType: nodeInfo['peer_type'],
    uptime: uptime,
    client: client,
    averagePing: _.sum(_.values(peerStats).map(n => parseInt(n['measured_latency']))) / peerCount,
    peersCount: peerCount,
    peersList: _.values(peerStats).map(n => n['node_id']),
    bestBlock: consensusStatus['bestBlock'],
    bestBlockHeight: consensusStatus['bestBlockHeight'],
    bestArrivedTime: consensusStatus['blockLastArrivedTime'],
    blockArrivePeriodEMA: consensusStatus['blockArrivePeriodEMA'],
    blockArrivePeriodEMSD: consensusStatus['blockArrivePeriodEMSD'],
    finalizedBlock: consensusStatus['lastFinalizedBlock'],
    finalizedBlockHeight: consensusStatus['lastFinalizedBlockHeight'],
    finalizedTime: consensusStatus['lastFinalizedTime'],
    finalizationPeriodEMA: consensusStatus['finalizationPeriodEMA'],
    finalizationPeriodEMSD: consensusStatus['finalizationPeriodEMSD'],
    packetsSent: packetsSent,
    packetsReceived: packetsReceived
  }

  if (nodeData.averagePing > 20000) {
    console.log('Enormous ping average from these hosts on ${nodeName}:')
    console.log(peerStats)
  }

  console.log('emitting: nodeInfo', nodeData)
  _.map(dashboards, dashboard => dashboard.emit('nodeInfo', nodeData))
};


interval(async () => {

  try {
    await main()
  }
  catch(err) {
    console.log('gRPC failed to connect to '+program.host+' with error: "' + err + '", retrying...')
  }

}, +defaultFrequency)


// @TODO do we remove this later, or is it unavoidable with node.js?
process.on('unhandledRejection', (err) => {
  console.error(err)
  process.exit(1)
})
