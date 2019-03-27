# Concordium Dashboard

POC dashboard to visualise the POC milestone. Elm frontend with small Typescript backend proxy.

Will become proper Dashboard monitor in future, likely with Scala backend.


### Usage

- `npm run dev` - Client and server are in watch mode with source maps, opens [http://localhost:3000](http://localhost:3000)
- `npm run test` - Runs jest tests
- `npm run build` - `dist` folder will include all the needed files, both client (Bundle) and server.
- `npm start` - Just runs `node ./dist/server/server.js`


### Getting some data

#### Use livenet data

Running local bakers requires a hefty build process, so if you just want quick data;

```
// Change dashboardHost in client.tsx
const dashboardHost = 'https://dashboard.eu.test.concordium.com/frontends'
```

#### Local bakers

You'll need the [p2p-client](https://gitlab.com/Concordium/p2p-client) repo and to follow the [local build instructions](https://gitlab.com/Concordium/p2p-client/tree/master/scripts/local).

Once you've got the images build, there is a helper script to help you boot collectors for each exposed baker port.

```
# Say we want to boot 5 bakers. Run this from the p2p-client/scripts/local folder;
NUM_BAKERS=5 docker-compose up --scale baker=5
# Then in another window, this from dashboard folder;
./runAllCollectors.sh
```

Now if you run `npm run dev` in a 3rd window to boot the dashboard frontend+backend, you should start getting live data from your local bakers via the booted collectors.

If that's hard to follow, here's an [architecture diagram](https://docs.google.com/drawings/d/1FWV8Ah9RAiqMaghT3Ql1JyGnBq0_TxOS6BgM6mFjepQ/edit) of what you're booting.


### Docker build

See `./docker.sh`, or run it. Builds `dist` locally first before copying into image. This is what gets used for the Kubernetes deploy.

---

#### Requirements

- Node 11+

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

Initially derived from [fullstack-typescript](https://github.com/gilamran/fullstack-typescript), replacing React with Elm.
