// rollup.config.js
import typescript from '@rollup/plugin-typescript';

export default {
  input: 'src/scamper.ts',
  output: {
    dir: 'dist',
    format: 'es',
    sourcemap: true,
  },
  plugins: [typescript()]
};