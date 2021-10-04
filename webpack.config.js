const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');

const packageJson = require('./package.json');
const analyticsId = 'G-TE81YY48VB';
const isProduction = process.env.NODE_ENV === 'production';
const middlewareUrl = process.env.CONCORDIUM_MIDDLEWARE_URL;
const collectorUrl = process.env.CONCORDIUM_COLLECTOR_BACKEND_URL;

// Checking if the webpack config is being used by dev server
const isDevServer = process.argv.some((v) => v.includes('webpack-dev-server'));

// Fail if missing the environment variables required in development
if (isDevServer && (!middlewareUrl || !collectorUrl)) {
  throw new Error(
    'Missing environment variables CONCORDIUM_MIDDLEWARE_URL and CONCORDIUM_COLLECTOR_BACKEND_URL for development',
  );
}

module.exports = {
  entry: './src/client',
  mode: isProduction ? 'production' : 'development',
  output: {
    filename: `[name]-[contenthash:8]-bundle.js`,
    publicPath: '/', // Will ensure deep links still fetches resources from the root.
    clean: true,
  },
  resolve: {
    extensions: ['.js', '.ts', '.elm'],
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: { loader: 'babel-loader' },
        exclude: /node_modules/,
      },
      {
        test: /.jpe?g$|.gif$|.png$|.svg$|.woff$|.woff2$|.ttf$|.eot$/,
        type: 'asset/resource',
      },
      {
        test: [/\.elm$/],
        exclude: /elm-stuff|node_modules/,
        use: {
          loader: 'elm-webpack-loader',
        },
      },
    ],
  },
  plugins: [
    new CopyPlugin({
      patterns: [{ from: 'public' }],
    }),
    new HtmlWebpackPlugin({
      template: './src/index.ejs',
      templateParameters: {
        title: 'Concordium Dashboard',
        analyticsId,
      },
    }),
    new webpack.DefinePlugin({
      __VERSION__: JSON.stringify(packageJson.version),
      __ANALYTICS_ID__: JSON.stringify(analyticsId),
      __PRODUCTION__: isProduction.toString(),
    }),
  ],
  devServer: {
    port: 3001,
    historyApiFallback: true,
    client: {
      overlay: true,
    },
    // Proxies the relevant requests to either the collector backend or the middleware.
    proxy: {
      '/nodesSummary': { target: collectorUrl, secure: false, changeOrigin: true },
      '/nodesBlocksInfo': { target: collectorUrl, secure: false, changeOrigin: true },
      '/v1': { target: middlewareUrl, secure: false, changeOrigin: true },
    },
  },
};
