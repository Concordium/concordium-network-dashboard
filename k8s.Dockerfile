FROM node:11

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# USER node

WORKDIR /home/node/app

# Possibly temporary, see proto/readme.md
COPY . .


ENV NODE_ENV="development"

RUN npm i
RUN npm run build

ENV NODE_ENV="production"
RUN npm install

ENV PORT=80
EXPOSE 80
ENTRYPOINT npm run start:prod
