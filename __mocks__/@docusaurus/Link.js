module.exports = {
  __esModule: true,
  default: ({ children, to, onClick, href }) => {
    const actualHref = to || href;
    return React.createElement('a', { href: actualHref, onClick }, children);
  },
};