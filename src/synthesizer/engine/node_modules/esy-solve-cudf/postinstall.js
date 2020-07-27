const fs = require('fs');
const path = require('path');

const platform = process.platform;
const destinationFilePath = path.join(__dirname, 'esySolveCudfCommand.exe');

const install = (filename) => {
  const srcFilename = path.join(__dirname, `platform-${platform}`, filename);
  const dstFilename = path.join(__dirname, filename);
  fs.renameSync(srcFilename, dstFilename);
}

switch (platform) {
  case 'linux':
  case 'darwin':
    install('esySolveCudfCommand.exe');
    fs.chmodSync(path.join(__dirname, 'esySolveCudfCommand.exe'), 0755);
    break;
  case 'win32':
    install('esySolveCudfCommand.exe');
    // chmod doesn't make sense on win32
    break;
  default:
    console.warn('[esy-solve-cudf] Unsupported operating system; dependent commands may not function correctly')
    break;
}
