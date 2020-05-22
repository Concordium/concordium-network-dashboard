# Concordium Dashboard

POC dashboard to visualise the POC milestone. Elm frontend with small Typescript backend proxy.

Will become proper Dashboard monitor in future, likely with Scala backend.


### Usage

- `npm run dev` - Client and server are in watch mode with source maps, opens [http://localhost:3000](http://localhost:3000)
- `npm run test` - Runs jest tests
- `npm run build` - `dist` folder will include all the needed files, both client (Bundle) and server.
- `npm start` - Just runs `node ./dist/server/server.js`


### Getting some data


Running local bakers requires a hefty build process, so if you just want quick data, you can force the config locally:

See `src/client/elm/Config.elm` for the different Environments supported. Change the config as such to force an Env:

```
// Change in 
config = 
  Staging
```

Note: the Production URLs are relative, so if you want to get live prod data locally for example, you'd update the `Production -> ""` definition to be `Production -> "https://dashboard.testnet.concordium.com/"` for example.


#### Local bakers

You'll need the [p2p-client](https://gitlab.com/Concordium/p2p-client) repo and to follow the [local build instructions](https://gitlab.com/Concordium/p2p-client/tree/master/scripts/local).

```
# Say we want to boot 5 bakers. Run this from the p2p-client/scripts/local folder;
EXTRA_ARGS="--debug --trace" NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.middleware.yml up --scale baker=5 --force-recreate
```

The dashboard collectors + backend has moved here:

https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector.rs
https://gitlab.com/Concordium/p2p-client/blob/develop/src/bin/collector_backend.rs

They are now part of the main build so running the docker network will have them report as per Config.elm/Local.


If that's hard to follow, here's an [architecture diagram](https://docs.google.com/drawings/d/1FWV8Ah9RAiqMaghT3Ql1JyGnBq0_TxOS6BgM6mFjepQ/edit) of what you're booting.


### Docker build

FYI this is what gets used for the Kubernetes deploy, you shouldn't need to run this but might be helpful if you're debugging CI or deploy issues.

See `./docker.sh`. Builds `dist` locally first before copying into image. 

---

#### Requirements

- Node 12+

---

### Directory Layout

```
.
├── /node_modules/          # 3rd-party libraries and utilities
├── /dist/                  # All the generated files will go here, and will run from this folder
├── /src/                   # The source code of the application
│   ├── /client/            # Elm app
│   ├── /server/            # Express server app
│   ├── /shared/            # Shared typescript code between the client and the server
├── /assets/                # images, css, json etc.
├── .babelrc                # babel configuration
├── .gitignore              # ignored git files and folders
├── .nvmrc                  # Force nodejs version
├── package.json            # The list of 3rd party libraries and utilities
└── tslint.json             # TypeScript linting configuration file
├── README.md               # This file
```

- Separate `tsconfig.json` for client and server.
- Client and server can share typescript code (and types)
- The client is bundled using [Webpack](https://webpack.github.io/) because it goes to the browser.
- The server is emitted by [TypeScript](https://github.com/Microsoft/TypeScript) because node 6 supports es6.

Note: server is for development aid only.

Initially derived from [fullstack-typescript](https://github.com/gilamran/fullstack-typescript), replacing React with Elm.
