FROM node:14 as build

# Minimum node version to include in summary calculations.
ARG stats_version

# Check that STATS_VERSION was indeed supplied
RUN : "${stats_version:?Must be set and non-empty.}"

ENV STATS_VERSION=${stats_version}

# https://github.com/nodejs/docker-node/blob/master/docs/BestPractices.md
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
# USER node

WORKDIR /home/node/app

# Install all of the npm dependencies
COPY ./package*.json ./
# Unsafe permission is needed for installing elm through npm
RUN npm ci --unsafe-perm=true

# Copy and build the rest of the project.
COPY . .
RUN npm run build

FROM nginx:alpine

COPY --from=build /home/node/app/dist/ /usr/share/nginx/html/
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
