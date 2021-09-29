FROM node:14 as build

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# USER node

WORKDIR /home/node/app

COPY . .

# Unsafe permission is needed for installing elm through npm
RUN npm install --unsafe-perm=true
RUN npm run build

FROM nginx:alpine

COPY --from=build /home/node/app/dist/ /usr/share/nginx/html/
RUN sed -i 's/index.*$/try_files \$uri \/index.html =404;/g' /etc/nginx/conf.d/default.conf

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
