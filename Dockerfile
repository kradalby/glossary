FROM node:7 as builder
WORKDIR /app

RUN npm install --silent -g elm@0.18.0

ADD package.json .
RUN npm install --silent

ADD elm-package.json .
RUN elm package install -y

ADD . .

RUN npm run build

FROM nginx:alpine 
COPY --from=builder /app/dist /usr/share/nginx/html
