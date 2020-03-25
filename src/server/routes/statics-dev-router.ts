const { createProxyMiddleware } = require('http-proxy-middleware');
import * as express from 'express';

export function staticsDevRouter() {
  const router = express.Router();
  router.use(express.json())

  // All the assets are hosted by Webpack on localhost:8090 (Webpack-dev-server)
  router.use('/public', createProxyMiddleware(
    {
      target: 'http://localhost:8090/'
    }));

  // @TODO need to figure out why normal assets aren't routing through webpack
  router.use('/assets', express.static('assets'))

  // Any route should render the web app html (hosted by by Webpack-dev-server)
  router.use('**', createProxyMiddleware(
    {
      target: 'http://localhost:8090/',
      pathRewrite: path => '/public/index.html',
    }));

  return router;
}
