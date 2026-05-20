module.exports = {
  __esModule: true,
  translate: (params, values) => {
    if (values && typeof values === 'object') {
      // Handle parameterized messages
      let message = params.message;
      Object.keys(values).forEach(key => {
        const value = values[key];
        message = message.replace(new RegExp(`\\{${key}\\}`, 'g'), value?.toString() || '');
      });
      return message;
    }
    return params.message;
  },
};