import typescript from '@rollup/plugin-typescript';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import {lezer} from "@lezer/generator/rollup"

export default {
  input: './src/web/ide.ts',
  output: {
    dir: './site/js',
    format: 'es',
    sourcemap: true,
  },
  plugins: [lezer(), typescript(), nodeResolve()]
};