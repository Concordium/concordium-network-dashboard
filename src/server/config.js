module.exports = {
  isProduction: process.env.NODE_ENV === 'production',
  serverPort: process.env.PORT || 3001
};
