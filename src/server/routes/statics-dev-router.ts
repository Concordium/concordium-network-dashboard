const { createProxyMiddleware } = require('http-proxy-middleware')
import * as express from 'express'

export default () => {
  const router = express.Router()

  router.use(express.json()) // TODO(mbo) Do we need this?

  // All the assets are hosted by Webpack on localhost:8090 (webpack-dev-server).
  router.use('/public', createProxyMiddleware({
    target: 'http://localhost:8090/',
  }))

  // @TODO need to figure out why normal assets aren't routing through webpack
  router.use('/assets', express.static('assets'))

  // Point any non-asset route to the app (hosted by Webpack dev server).
  router.use('**', createProxyMiddleware({
    target: 'http://localhost:8090/',
    pathRewrite: path => '/public/index.html',
  }))

  return router
}
