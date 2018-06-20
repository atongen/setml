const path = require("path");

const isProd = process.env.NODE_ENV === "production";

module.exports = {
  entry: {
    index: "./lib/js/src/client/Index.js",
    game: "./lib/js/src/client/Game.js",
  },
  mode: isProd ? "production" : "development",
  output: {
    path: path.join(__dirname, "public/js"),
    filename: "[name].js",
  },
};
