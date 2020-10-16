const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const OpenBrowserPlugin = require('open-browser-webpack-plugin');

const config = require('./src/server/config');
const nodeModulesPath = path.resolve(__dirname, 'node_modules');

const plugins = [
  new HtmlWebpackPlugin({
    filename: 'index.html',
    template: './src/client/index.ejs',
    templateParameters: {
        title: 'Concordium Dashboard',
        isProduction: config.isProduction,
    },
  }),
];

if (!config.isProduction) {
  plugins.push(
    new OpenBrowserPlugin({ url: `http://localhost:${config.serverPort}` }),
  );
}

module.exports = {
  mode: config.isProduction ? 'production' : 'development',
  devtool: config.isProduction ? '' : 'inline-source-map',
  entry: ['@babel/polyfill', './src/client/client'],
  output: {
    path: path.join(__dirname, 'dist', 'public'),
    filename: `[name]-[hash:8]-bundle.js`,
    publicPath: config.isProduction ? '/' : '/public/',
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
  devServer: {
    port: 8090
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
            options: config.isProduction ? { optimize: true } : { debug: true, forceWatch: true }
          }
        ]
      }
    ],
  },
  plugins
};
