const path = require("path");

const isProd = process.env.NODE_ENV === "production";

module.exports = {
  entry: {
    index: "./lib/es6/src/client/Index.js",
    game: "./lib/es6/src/client/Game.js",
  },
  mode: isProd ? "production" : "development",
  output: {
    path: path.join(__dirname, "public/js"),
    filename: "[name].js",
  },
};
