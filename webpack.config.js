const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyPlugin = require('copy-webpack-plugin');
const webpack = require('webpack');

const packageJson = require('./package.json');
const analyticsId = 'G-TE81YY48VB';
const isProduction = process.env.NODE_ENV === 'production';

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
  },
};
