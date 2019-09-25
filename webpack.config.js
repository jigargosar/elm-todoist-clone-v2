const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')

// https://webpack.js.org/configuration/
module.exports = {
  mode: 'development',
  entry: { index: './src/index.js' },
  output: {
    publicPath: '/',
    path: path.resolve(__dirname, 'build'),
  },
  resolve: {
    extensions: ['.js', '.elm', '.ts', '.tsx'],
  },
  module: {
    rules: [
      { test: /\.css$/, loader: ['style-loader', 'css-loader'] },
      { test: /\.tsx?$/, loader: ['ts-loader'] },
      {
        test: /\.elm$/,
        use: ["elm-hot-webpack-loader",
          {
            loader: 'elm-webpack-loader',
            options: { optimize: false, debug: true },
          },
        ],
      },
    ],
  },
  plugins: [new HtmlWebpackPlugin({ template: './src/index.html' })],
  optimization: {
    // splitChunks: false,
    // runtimeChunk: true,
    splitChunks: {
      cacheGroups: {
        vendors: {
          test: /[\\/]node_modules[\\/]/,
        }
      }
    }
  },
  // https://webpack.js.org/configuration/stats/
  // stats: 'errors-warnings',
  stats: {
    children: false,
    modules: false,
  },
  // devtool: isProduction ? 'source-map' : 'eval-source-map',
  // devtool: isProduction ? 'source-map' : false,
  devtool: false,
  // https://webpack.js.org/configuration/dev-server/
  devServer: {
    historyApiFallback: true,
    overlay: {
      warnings: true,
      errors: true,
    },
    hot: false,
    // hotOnly: true,
  },
}
