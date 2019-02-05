FROM node:11

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# USER node

WORKDIR /home/node/app

# Possibly temporary, see proto/readme.md
COPY docs docs
COPY elm.json elm.json
COPY index.js index.js
COPY package.json package.json
COPY package-lock.json package-lock.json
COPY proto proto
COPY src src
COPY tsconfig.json tsconfig.json
COPY tslint.json tslint.json
COPY types.d.ts types.d.ts
COPY webpack.config.js webpack.config.js


ENV NODE_ENV="development"

RUN npm i
RUN npm run build

ENV NODE_ENV="production"
RUN npm install

ENV PORT=80
EXPOSE 80
ENTRYPOINT npm run start:prod
