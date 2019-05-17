#!/bin/bash

PATH=/home/node/.npm-global/bin:$PATH

mkdir -p ~/.ssh
cp /etc/deploy-key/k8s ~/.ssh/id_ed25519
chmod 0400 ~/.ssh/id_ed25519

ssh-keyscan -H gitlab.com >> ~/.ssh/known_hosts
git clone git@gitlab.com:Concordium/visualizations.git

cd visualizations

NODE_ENV=development npm i

NODE_ENV=production npm install
NODE_ENV=production npm run build

su - node -c 'cd visualizations && NODE_ENV=production npm run start:prod'
