FROM node:11

EXPOSE 8080

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# USER node

WORKDIR /home/node/

COPY startup-k8s.sh .


ENTRYPOINT ./startup-k8s.sh
