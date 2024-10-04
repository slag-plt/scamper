import typescript from '@rollup/plugin-typescript';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import {lezer} from "@lezer/generator/rollup"
import commonjs from '@rollup/plugin-commonjs';

export default [
  {
    input: './src/web/file-chooser.ts',
    output: {
      file: `./site/js/scamper-file-chooser-${process.env.npm_package_version}.js`,
      format: 'es',
      sourcemap: true,
    },
    plugins: [lezer(), typescript(), nodeResolve(), commonjs()]
  },
  {
    input: './src/web/ide.ts',
    output: {
      file: `./site/js/scamper-ide-${process.env.npm_package_version}.js`,
      format: 'es',
      sourcemap: true,
    },
    plugins: [lezer(), typescript(), nodeResolve(), commonjs()]
  },
  {
    input: './src/web/runner.ts',
    output: {
      file: `./site/js/scamper-runner-${process.env.npm_package_version}.js`,
      format: 'es',
      sourcemap: true,
    },
    plugins: [typescript(), nodeResolve(), commonjs()]
  },
  {
    input: './src/web/web.ts',
    output: {
      file: `./site/js/scamper-web-${process.env.npm_package_version}.js`,
      format: 'es',
      sourcemap: true,
    },
    plugins: [typescript(), nodeResolve(), commonjs()]
  }
]