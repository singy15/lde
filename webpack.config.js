const path = require("path");
const src = __dirname + "/src";
const glob = require("glob");

var webpack = require("webpack");
const HtmlWebpackPlugin = require('html-webpack-plugin');

const entry = glob.sync("js/*.js")
  .reduce((x, y) => Object.assign(x, { [(y.substring(3)).replace("\.js", "")]: "./" + y }), {});
console.log(entry);

const plugins = glob.sync("html/*.ejs")
  .reduce((x, y) => { 
    x.push(new HtmlWebpackPlugin({
      template: y,
      filename: "../../templates/" + y.substring(5).replace("\.ejs", ".html"),
      chunks: [(y.substring(5)).replace("\.ejs", "")],
      hash: true,
      inject: false
    })); 
    return x; 
  }, []);
console.log(plugins);

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
  entry: entry,
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
  plugins: plugins,
};

