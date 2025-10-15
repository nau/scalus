module.exports = require('./scalajs.webpack.config');
module.exports.output.libraryTarget = 'commonjs2';
module.exports.externals = {
  fs: 'commonjs fs'
};
