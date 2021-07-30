FROM node:14 as build

# See 'https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md'.
WORKDIR /home/node/app

COPY package.json yarn.lock ./
RUN yarn install

COPY . .
RUN NODE_ENV="production" yarn build

FROM nginx:alpine
COPY --from=build /home/node/app/dist/public /usr/share/nginx/html/
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
