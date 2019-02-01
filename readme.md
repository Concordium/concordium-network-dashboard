# Concordium Dashboard

### Usage

- `npm run dev` - Client and server are in watch mode with source maps, opens [http://localhost:3000](http://localhost:3000)
- `npm run test` - Runs jest tests
- `npm run build` - `dist` folder will include all the needed files, both client (Bundle) and server.
- `npm start` - Just runs `node ./dist/server/server.js`

### Docker build

See `./docker.sh`, or run it. Builds `dist` locally first before copying into image.

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
