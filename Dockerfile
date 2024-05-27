# Build a node-based static site
FROM node:16 AS build
WORKDIR /app
COPY package.json yarn.lock .nvmrc /app/
RUN yarn --frozen-lockfile
# `.dockerignore` is important to cache this copy properly
COPY . /app/
RUN yarn test
RUN yarn build

# Run the static site we just built. No Caddyfile or other config, just static files.
# "The default config file simply serves files from /usr/share/caddy" - https://hub.docker.com/_/caddy
FROM caddy:2.8
COPY --from=build /app/build/ /usr/share/caddy