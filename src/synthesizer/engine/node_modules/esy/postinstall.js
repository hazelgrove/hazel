/**
 * release-postinstall.js
 *
 * XXX: We want to keep this script installable at least with node 4.x.
 *
 * This script is bundled with the `npm` package and executed on release.
 * Since we have a 'fat' NPM package (with all platform binaries bundled),
 * this postinstall script extracts them and puts the current platform's
 * bits in the right place.
 */

var path = require('path');
var cp = require('child_process');
var fs = require('fs');
var os = require('os');
var platform = process.platform;

const binariesToCopy = [
  path.join('_build', 'default', 'bin', 'esy.exe'),
  path.join('_build', 'default', 'bin', 'esyInstallRelease.js'),
  path.join(
    '_build',
    'default',
    'esy-build-package',
    'bin',
    'esyBuildPackageCommand.exe'
  ),
  path.join(
    '_build',
    'default',
    'esy-build-package',
    'bin',
    'esyRewritePrefixCommand.exe'
  )
];

/**
 * Since os.arch returns node binary's target arch, not
 * the system arch.
 * Credits: https://github.com/feross/arch/blob/af080ff61346315559451715c5393d8e86a6d33c/index.js#L10-L58
 */

function arch() {
  /**
   * The running binary is 64-bit, so the OS is clearly 64-bit.
   */
  if (process.arch === 'x64') {
    return 'x64';
  }

  /**
   * All recent versions of Mac OS are 64-bit.
   */
  if (process.platform === 'darwin') {
    return 'x64';
  }

  /**
   * On Windows, the most reliable way to detect a 64-bit OS from within a 32-bit
   * app is based on the presence of a WOW64 file: %SystemRoot%\SysNative.
   * See: https://twitter.com/feross/status/776949077208510464
   */
  if (process.platform === 'win32') {
    var useEnv = false;
    try {
      useEnv = !!(process.env.SYSTEMROOT && fs.statSync(process.env.SYSTEMROOT));
    } catch (err) {}

    var sysRoot = useEnv ? process.env.SYSTEMROOT : 'C:\\Windows';

    // If %SystemRoot%\SysNative exists, we are in a WOW64 FS Redirected application.
    var isWOW64 = false;
    try {
      isWOW64 = !!fs.statSync(path.join(sysRoot, 'sysnative'));
    } catch (err) {}

    return isWOW64 ? 'x64' : 'x86';
  }

  /**
   * On Linux, use the `getconf` command to get the architecture.
   */
  if (process.platform === 'linux') {
    var output = cp.execSync('getconf LONG_BIT', {encoding: 'utf8'});
    return output === '64\n' ? 'x64' : 'x86';
  }

  /**
   * If none of the above, assume the architecture is 32-bit.
   */
  return 'x86';
}

// implementing it b/c we don't want to depend on fs.copyFileSync which appears
// only in node@8.x
function copyFileSync(sourcePath, destPath) {
  const data = fs.readFileSync(sourcePath);
  const stat = fs.statSync(sourcePath);
  fs.writeFileSync(destPath, data);
  fs.chmodSync(destPath, stat.mode);
}

const copyPlatformBinaries = platformPath => {
  const platformBuildPath = path.join(__dirname, 'platform-' + platformPath);

  binariesToCopy.forEach(binaryPath => {
    const sourcePath = path.join(platformBuildPath, binaryPath);
    const destPath = path.join(__dirname, binaryPath);
    if (fs.existsSync(destPath)) {
      fs.unlinkSync(destPath);
    }
    copyFileSync(sourcePath, destPath);
    fs.chmodSync(destPath, 0o755);
  });
};

switch (platform) {
  case 'win32':
    if (arch() !== 'x64') {
      console.warn('error: x86 is currently not supported on Windows');
      process.exit(1);
    }

    copyPlatformBinaries('win32');

    console.log('Installing native compiler toolchain for Windows...');
    cp.execSync(`npm install esy-bash@0.3.19 --prefix "${__dirname}"`);
    console.log('Native compiler toolchain installed successfully.');
    break;
  case 'linux':
  case 'darwin':
    copyPlatformBinaries(platform);
    break;
  default:
    console.warn('error: no release built for the ' + platform + ' platform');
    process.exit(1);
}
