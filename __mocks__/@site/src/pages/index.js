module.exports = {
  getInstallTarget: jest.fn((version, suffix) => 
    `https://github.com/alire-project/alire/releases/download/v${version}/alr-${suffix}`
  ),
  gitHubReleasePage: 'https://github.com/alire-project/alire/releases',
  installTargets: new Map([
    ['linux', { label: 'Linux', urlSuffix: 'linux' }],
    ['macos', { label: 'macOS', urlSuffix: 'macos' }],
    ['windows', { label: 'Windows', urlSuffix: 'windows' }]
  ])
};