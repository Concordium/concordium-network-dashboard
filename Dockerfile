FROM node:14 as build

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NODE_ENV="production"
WORKDIR /home/node/app

COPY package.json yarn.lock ./
RUN yarn
COPY . .
RUN yarn build

FROM nginx:alpine
COPY --from=build /home/node/app/dist/public /usr/share/nginx/html/
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
