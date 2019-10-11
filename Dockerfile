FROM node:11

ENV NODE_ENV="production"

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# USER node

WORKDIR /home/node/app

COPY dist dist
# Possibly temporary, see proto/readme.md
COPY proto proto
COPY package.json package.json
COPY package-lock.json package-lock.json
COPY dashboard-backend.js dashboard-backend.js

RUN npm install concurrently
RUN npm install

ENV PORT=80
EXPOSE 80
ENTRYPOINT npm run start:prod
