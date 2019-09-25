import * as proxy from 'http-proxy-middleware';
import * as express from 'express';

export function staticsDevRouter(nodesSummary) {
  const router = express.Router();
  router.use(express.json()) 

// All the assets are hosted by Webpack on localhost:8080 (Webpack-dev-server)
  router.use('/public', proxy(
    {
      target: 'http://localhost:8080/'
    }));

  // @TODO need to figure out why normal assets aren't routing through webpack
  router.use('/assets', express.static('assets'))

  router.post('/post/nodes', function(request, response){
    nodesSummary[request.body.nodeName] = request.body;
    response.status(200).end()
  });

// Any route should render the web app html (hosted by by Webpack-dev-server)
  router.use('**', proxy(
    {
      target: 'http://localhost:8080/',
      pathRewrite: path => '/public/index.html',
    }));

  return router;
}
