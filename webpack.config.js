const path = require("path");

const isProd = process.env.NODE_ENV === "production";
const webpack = require('webpack');
const CompressionPlugin = require('compression-webpack-plugin');
const ManifestPlugin = require('webpack-manifest-plugin');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');

module.exports = {
    plugins: [
        new webpack.HashedModuleIdsPlugin(),
        new CompressionPlugin(),
        new ManifestPlugin({})
    ],
    entry: {
        index: "./lib/js/src/client/Index.js",
        game: "./lib/js/src/client/Game.js",
    },
    mode: isProd ? "production" : "development",
    output: {
        path: path.join(__dirname, "public/assets"),
        filename: isProd ? "[name].[contenthash].js" : "[name].js",
    },
    optimization: {
        minimizer: isProd ? [new UglifyJsPlugin({sourceMap: true})] : [],
        runtimeChunk: "single",
        splitChunks: {
            cacheGroups: {
                vendor: {
                    test: /[\\/]node_modules[\\/]/,
                    name: 'vendors',
                    chunks: 'all'
                },
                pathData: {
                    test: /path_data/,
                    name: 'path_data',
                    chunks: 'all'
                }
            }
        }
    }
};
