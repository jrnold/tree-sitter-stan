/* eslint-disable global-require,import/no-unresolved */
try {
  module.exports = require('./build/Release/tree_sitter_stan_binding');
} catch (error) {
  try {
    module.exports = require('./build/Debug/tree_sitter_stan_binding');
  } catch (_) {
    throw error;
  }
}
