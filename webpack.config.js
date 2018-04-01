const path = require('path');

const isProd = process.env.NODE_ENV === 'production';

module.exports = {
  entry: {
    app: './lib/js/src/client/App.js',
  },
  mode: isProd ? 'production' : 'development',
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: 'public/js/[name].js',
  },
};
