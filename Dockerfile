FROM node:14-alpine

WORKDIR /home/node/app

RUN apk add bash
RUN apk add yarn

RUN npm install --global increase-memory-limit
RUN increase-memory-limit
RUN npm install --global concurrently
RUN npm install --global react-scripts
RUN npm install --global nodemon

COPY . ./

RUN rm -Rf build
RUN rm -Rf node_modules

RUN yarn install
RUN yarn run build-client

RUN chown -R node:node /home/node/app
RUN mkdir -p node_modules/.cache
RUN chmod -R 777 node_modules/.cache

EXPOSE 5000

CMD ["yarn", "start"]