const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const OpenBrowserPlugin = require('open-browser-webpack-plugin');

const config = require('./src/server/config');
const nodeModulesPath = path.resolve(__dirname, 'node_modules');

const MODE =
  config.IS_PRODUCTION ? 'production' : 'development'

const plugins = [
  new HtmlWebpackPlugin({
    title: 'Concordium Dashboard',
    filename: 'index.html',
    template: './src/client/index.ejs',
  }),
];

//const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;
//plugins.push(new BundleAnalyzerPlugin());

if (!config.IS_PRODUCTION) {
  plugins.push(
    new OpenBrowserPlugin({ url: `http://localhost:${config.SERVER_PORT}` }),
  );
}

module.exports = {
  mode: config.IS_PRODUCTION ? 'production' : 'development',
  devtool: config.IS_PRODUCTION ? '' : 'inline-source-map',
  entry: ['@babel/polyfill', './src/client/client'],
  output: {
    path: path.join(__dirname, 'dist', 'public'),
    filename: `[name]-[hash:8]-bundle.js`,
    publicPath: config.IS_PRODUCTION ? '/' : '/public/'
  },
  resolve: {
    extensions: ['.js', '.ts', '.tsx', '.elm'],
  },
  optimization: {
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendors',
          chunks: 'all',
        },
      },
    },
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loaders: ['babel-loader'],
        exclude: [/node_modules/, nodeModulesPath],
      },
      {
        test: /.jpe?g$|.gif$|.png$|.svg$|.woff$|.woff2$|.ttf$|.eot$/,
        use: 'url-loader?limit=10000',
      },
      {
        test: [/\.elm$/],
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "elm-webpack-loader",
            options:
              MODE === "production" ? {} : { debug: true, forceWatch: true }
          }
        ]
      }
    ],
  },
  plugins
};
