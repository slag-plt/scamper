import typescript from '@rollup/plugin-typescript';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import {lezer} from "@lezer/generator/rollup"

export default {
  input: './src/web/ide.ts',
  output: {
    file: `./site/js/scamper-ide-${process.env.npm_package_version}.js`,
    format: 'es',
    sourcemap: true,
  },
  plugins: [lezer(), typescript(), nodeResolve()]
};