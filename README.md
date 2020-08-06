# Concordium Dashboard

Network and Chain Dashboard for displaying stats, summaries, and visualizations of the blockchain network and state.

- Network: A table view of nodes which are participating in the data collection.
  Clicking a node in the table opens a full screen view with the details of that node.
- Chain: A live visualization of the chain which displays new blocks as they're being submitted to the network.
  The blocks are colored based on their finalization status and a number of dots illustrate the
  consensus that has been reached around a given block.
  Clicking a block displays the contents of the block in the Node Explorer below the visualization.

The app consists of a server written in [TypeScript](https://github.com/Microsoft/TypeScript)
and a client written in [Elm](https://elm-lang.org/) (and packaged using [webpack](https://webpack.github.io/)).
The client is compiled into a few HTML and JavaScript files.
The server is very basic and just registers a router for serving these static files.


### Install and run

From the project root, run
```
npm install
```

To build and run the app in watch mode with source maps, run
```
npm run dev
```

This also opens [http://localhost:3000](http://localhost:3000) in a browser and is the preferred method of
running the app during development.

Other common build/run targets include:
  
- `npm test` - Runs jest tests (currently fails because there are no tests!).
- `npm run build` - Builds the app in production mode and puts it into `./dist`.
- `npm start` - Start the app (shorthand for `node ./dist/server/server.js`).

See the `script` section of `package.json` for all targets as well as their definitions.


### Configuration

The client app may be built in production or development mode (based on the `NODE_ENV` environment variable).
In production mode (which includes staging), the collector/middleware backends are assumed to reside on the same 
domain as the dashboard itself. In development mode, the target backend (local, staging, or production) may be set
by changing the value of `devTarget` in `src/client/elm/Config.elm`.


### Architecture

The dashboard collects data from two different backend services (see [dependencies](#dependencies)):

- Collector Backend: All current state (participating nodes and their stats, including their best block etc.).
  This is polled periodically.
- Middleware: Block contents for the Block Explorer.

Note that the dashboard server doesn't serve any dynamic data - only the frontend application itself.

A complete setup consists of ([diagram](https://docs.google.com/drawings/d/1FWV8Ah9RAiqMaghT3Ql1JyGnBq0_TxOS6BgM6mFjepQ/edit)):

- One or more nodes.
- One collector for each node that we want to participate data. This collector polls the node and pushes the state to the collector backend.
- One collector backend for keeping the state from the collectors. This is the components that the dashboard client polls.
- One middleware instance connected to one of the nodes.


#### Local setup

Follow the instructions at [p2p-client](https://gitlab.com/Concordium/p2p-client) to set up a local node/baker.

The easiest solution is to start a local cluster using the docker-compose scripts.
This will contain all components except for the middleware. There are different scripts for different cluster sizes.

The middleware resides in the [simple-client](https://gitlab.com/Concordium/consensus/simple-client/) repo
and may be started (from the root of that project) using the command
```
NODE_URL=127.0.0.1:$PORT stack run middleware
```
where `$PORT` is the GRPC port of the node (default: 10000).

If the docker-compose scripts are used, then `scripts/bootMiddleware.sh` will run the above command with a working port.

The node and [collector](https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector.rs)
([backend](https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector_backend.rs)) is defined in the
[p2p-client](https://gitlab.com/Concordium/p2p-client) repo.

Note that you need to set a feature flag to build the collector (backend).


### Docker build

The Dashboard is deployed on Kubernetes using a dockerized build which is build by `./docker.sh`.
Builds `dist` locally first before copying into image. 


#### Requirements and credits

- Node 12+

- The project structure was initially derived from [fullstack-typescript](https://github.com/gilamran/fullstack-typescript),
replacing React with Elm.
