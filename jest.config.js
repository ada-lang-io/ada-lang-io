/** @type {import("jest").Config} */
module.exports = {
  testEnvironment: "jsdom",
  transform: {
    '^.+\\.(ts|tsx)$': [
      'ts-jest',
      {
        tsconfig: "./jest.tsconfig.json"
      }
    ],
    // For other files, use default transforms
    '^.+\\.(js|jsx)$': 'babel-jest',
  },
  moduleNameMapper: {
    "^@site/(.*)$": "<rootDir>/$1",
    "^@theme/(.*)$": "<rootDir>/src/theme/$1",
    "^@docusaurus/(.*)$": "<rootDir>/node_modules/@docusaurus/$1",
    "^@theme-original/(.*)$": "<rootDir>/node_modules/@docusaurus/theme-classic/lib/theme/$1",
    "\\.module\\.scss$": "identity-obj-proxy",
    "\\.scss$": "identity-obj-proxy",
    "\\.css$": "identity-obj-proxy",
  },
  setupFilesAfterEnv: ["<rootDir>/setupTests.js"],
  testPathIgnorePatterns: ["/node_modules/", "/setupTests\\.js$/"],
};