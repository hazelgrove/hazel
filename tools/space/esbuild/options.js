import { existsSync, rmSync, copyFileSync, mkdirSync } from 'node:fs';
import { createRequire } from 'node:module';
import process from 'node:process';
import { execSync } from 'node:child_process';
import externals from '@inkandswitch/patchwork-bootloader/externals';

const pushworking = process.argv.includes('pushwork') || process.env.PUSHWORK;

/** @type {import("esbuild").Plugin[]} */
const plugins = [
  {
    name: 'empty outdir',
    setup(build) {
      build.onStart(() => {
        const { outdir } = build.initialOptions;
        if (outdir && existsSync(outdir)) rmSync(outdir, { recursive: true });
      });
    },
  },
  {
    name: 'copy-tldraw-css',
    setup(build) {
      build.onEnd(() => {
        const require = createRequire(import.meta.url);
        const src = require.resolve('tldraw/tldraw.css');
        const { outdir } = build.initialOptions;
        mkdirSync(outdir, { recursive: true });
        copyFileSync(src, `${outdir}/tldraw.css`);
      });
    },
  },
];

if (pushworking) {
  plugins.push({
    name: 'pushwork',
    setup(build) {
      if (!existsSync('.pushwork')) {
        console.warn('no .pushwork directory! run `pushwork init .` first');
        return;
      }
      build.onEnd((result) => {
        if (result.errors.length) {
          console.warn('esbuild errors! skipping pushwork sync');
          return;
        }
        try {
          execSync('pushwork sync', { stdio: 'inherit' });
        } catch (error) {
          console.warn(error.message);
        }
      });
    },
  });
}

/** @type {import("esbuild").BuildOptions} */
export default {
  entryPoints: ['src/index.ts'],
  outdir: 'dist',
  bundle: true,
  platform: 'browser',
  format: 'esm',
  splitting: true,
  logLevel: 'debug',
  sourcemap: false,
  jsx: 'automatic',
  jsxImportSource: 'react',
  external: externals,
  minify: false,
  plugins,
  define: {
    'process.env.NODE_ENV': JSON.stringify('development'),
  },
};
