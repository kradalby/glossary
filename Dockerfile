FROM node:10 as builder
WORKDIR /app

RUN yarn global add elm@0.18.0

ADD package.json .
RUN yarn install

ADD elm-package.json .
RUN elm package install -y

ADD . .

RUN npm run build

FROM nginx:alpine 
COPY --from=builder /app/dist /usr/share/nginx/html
