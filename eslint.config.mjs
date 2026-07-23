import eslint from '@eslint/js'
import { defineConfig, globalIgnores } from 'eslint/config'
import tseslint from 'typescript-eslint'
import globals from 'globals'
import eslintConfigPrettier from 'eslint-config-prettier/flat'
import 'eslint-plugin-only-warn'
import vueEslint from 'eslint-plugin-vue'

export default defineConfig(
  eslint.configs.recommended,
  tseslint.configs.strictTypeChecked,
  tseslint.configs.stylisticTypeChecked,
  ...vueEslint.configs['flat/recommended-error'],
  {
    files: ['*.vue', '**/*.vue'],
    languageOptions: {
      parserOptions: {
        parser: tseslint.parser,
      },
      globals: {
        ...globals.browser,
        APP_VERSION: 'readonly',
      },
    },
  },
  eslintConfigPrettier,
  [
    globalIgnores([
      'dist/*',
      'types/*',
      'coverage/*',
      'src/js/webaudiofont/*',
      'src/scheme/generated/*',
      'src/lib/generated/*',
    ]),
  ],
  {
    languageOptions: {
      parserOptions: {
        projectService: {
          allowDefaultProject: [
            'eslint.config.mjs',
            'vite.config.ts',
            'scripts/generate-parser.mjs',
            'scripts/vite-plugin-scheme-parser.mjs',
            'scripts/generate-lib-sources.mjs',
            'scripts/vite-plugin-lib-sources.mjs',
            'scripts/vite-plugin-flatten-html.mjs',
            'scripts/vite-plugin-dev-root-redirect.mjs',
          ],
        },
        tsconfigRootDir: import.meta.dirname,
        extraFileExtensions: ['.vue'],
      },
    },
  },
  {
    files: [
      'vite.config.ts',
      'scripts/generate-parser.mjs',
      'scripts/vite-plugin-scheme-parser.mjs',
      'scripts/generate-lib-sources.mjs',
      'scripts/vite-plugin-lib-sources.mjs',
      'scripts/vite-plugin-flatten-html.mjs',
      'scripts/vite-plugin-dev-root-redirect.mjs',
    ],
    languageOptions: {
      globals: {
        ...globals.node,
      },
    },
  },
  {
    rules: {
      '@typescript-eslint/no-unused-vars': [
        'warn',
        {
          argsIgnorePattern: '^_',
          varsIgnorePattern: '^_',
          caughtErrorsIgnorePattern: '^_',
        },
      ],
      quotes: ['warn', 'single', { avoidEscape: true }],
      'lines-between-class-members': [
        'warn',
        'always',
        { exceptAfterSingleLine: true },
      ],
      'padding-line-between-statements': [
        'warn',
        { blankLine: 'always', prev: 'function', next: '*' },
        { blankLine: 'always', prev: '*', next: 'function' },
      ],
      'no-restricted-syntax': [
        'warn',
        {
          selector: 'PrivateIdentifier',
          message:
            'Use the `private` modifier instead of `#` to enforce privacy.',
        },
      ],
    },
  },
)
