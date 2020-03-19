FROM node:11 as build

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# USER node

WORKDIR /home/node/app

# Possibly temporary, see proto/readme.md
COPY . .

ENV NODE_ENV="production"
RUN NODE_ENV="development" npm install
RUN NODE_ENV="development" npm install concurrently
RUN npm install
RUN npm install concurrently
RUN npm run build

FROM nginx:alpine

COPY --from=build /home/node/app/dist/public /usr/share/nginx/html/
RUN sed -i 's/index.*$/try_files \$uri \/index.html =404;/g' /etc/nginx/conf.d/default.conf

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
