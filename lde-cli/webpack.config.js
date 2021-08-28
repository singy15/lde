const path = require("path");
const glob = require("glob");
const CopyPlugin = require("copy-webpack-plugin");

var webpack = require("webpack");
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  mode: "development",
  devtool: "source-map",
  devServer: {
    // open: true,
    // openPage: "index.html",
    contentBase: path.join(__dirname, "document-root", "public"),
    watchContentBase: true,
    port: 8080
  },
  entry: {
    index: "./js/index.js"
    ,editor: "./js/editor.js"
  },
  output: {
    path: path.join(__dirname, "document-root", "public"),
    publicPath: "/public/",
    filename: "js/[name].js",
    library: ["com", "kedama"],
    libraryTarget: "umd"
  },
  module: {
    rules: [
      {
        test: /\.scss$/,
        use: [
          "style-loader",
          "css-loader",
          "sass-loader"
        ]
      },
      {
        test: /\.css$/,
        use: [
          "style-loader",
          "css-loader"
        ]
      },
      {
        test: /\.html$/,
        use: [
          "html-loader"
        ]
      }
    ]
  },
  resolve: {
    alias: {
      vue: "vue/dist/vue.js"
    }
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: "html/index.ejs",
      filename: "../../templates/index.html",
      chunks: ["index"],
      hash: true,
      inject: false
    })
    ,new HtmlWebpackPlugin({
      template: "html/editor.ejs",
      filename: "../../templates/editor.html",
      chunks: ["editor"],
      hash: true,
      inject: false
    })
    ,new CopyPlugin({
      patterns: [
        { from: 'document-root', to: "../../dist/document-root" } 
        ,{ from: 'templates', to: "../../dist/templates" } 
        // ,{ from: 'src/public', to: "public" } 
        // ,{ from: 'src/favicon.ico', to: "public/favicon.ico" } 

        // Deploy for wasp_cl
        ,{ from: 'document-root', to: "../../../lde-web/document-root" } 
        ,{ from: 'templates', to: "../../../lde-web/templates" } 
        // ,{ from: 'src/public', to: "../../wasp_cl/document-root/public" } 
        // ,{ from: 'dist/public', to: "../../wasp_cl/document-root/public" } 
        // ,{ from: 'src/html', to: "../../wasp_cl/templates" } 
      ],
      options: {
        concurrency: 1
      }
    })
  ]
};

