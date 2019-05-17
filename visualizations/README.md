### Concordium Website

https://concordium.com

### Local development

```
npm install
npm run dev
```

Client and server are in watch mode with source maps and recompiles + refreshes automatically on any file changes, opens [http://localhost:3000](http://localhost:3000) the first time run.

### Production dev

```
NODE_ENV=production npm run build && NODE_ENV=production npm run start:prod
```

- `npm run build` - `dist` folder will include all the needed files, both client (Bundle) and server.
- `npm start` - Just runs `node ./dist/server/server.js`

Note: webpack doesn't have asset management (images etc) support for elm-0.19, so it is done manually by placing `assets` into `public/assets` on build command, see package.json `build-client` definition.

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
├── .gitignore              # ignored git files and folders
├── package.json            # The list of 3rd party libraries and utilities
├── elm.json                # The list of 3rd party Elm libraries and utilities
└── tslint.json             # TypeScript linting configuration file
├── README.md               # This file
```

- Separate `tsconfig.json` for client and server.
- Client and server can share typescript code (and types)
- The client is bundled using [Webpack](https://webpack.github.io/) because it goes to the browser.
- The server is emitted by [TypeScript](https://github.com/Microsoft/TypeScript) because node 6 supports es6.

Initially derived from [fullstack-typescript](https://github.com/gilamran/fullstack-typescript), replacing React with Elm.
