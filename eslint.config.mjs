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
      // Agent worktrees (git worktrees created under .claude/) are separate
      // checkouts; never lint into them.
      '.claude/**',
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
            'scripts/vite-plugin-dev-flat-html.mjs',
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
    files: ['server/**/*.ts'],
    languageOptions: {
      globals: {
        ...globals.node,
      },
    },
  },
  // The client/server boundary. `server/` is a workspace of this repo, so npm
  // hoists its dependencies into the root node_modules and nothing physically
  // stops a Vue component from importing `better-auth`. These two rules are
  // what keep the split real rather than merely conventional.
  {
    files: ['src/**/*.ts', 'src/**/*.vue'],
    rules: {
      '@typescript-eslint/no-restricted-imports': [
        'warn',
        {
          patterns: [
            {
              group: ['**/server', '**/server/**'],
              message:
                'The client must not import server code. The two share only the FS contract in src/fs/fs.ts, which the server imports from here -- not the other way around.',
            },
          ],
        },
      ],
    },
  },
  {
    files: ['server/**/*.ts'],
    rules: {
      '@typescript-eslint/no-restricted-imports': [
        'warn',
        {
          patterns: [
            {
              // Type-only imports are erased at compile time, so they create no
              // runtime coupling and cannot drag browser code into the server.
              //
              // src/fs/fs.ts is excepted outright, values included: it is the
              // contract both halves implement -- the FS interface, the
              // FileEntry shape, and the predicates deciding what counts as a
              // user's own file -- and sharing it is what stops the backends
              // drifting on questions like what "hidden" means. It is safe
              // because it is pure: no DOM, no browser API. The server's
              // tsconfig omits the DOM lib, so if that ever stops being true
              // the server typecheck fails rather than shipping.
              // The browser-side areas of src/, named one by one. This rule's
              // globs have no working negation and their `*` crosses `/`, so
              // "all of src/ except the shared contract" cannot be written
              // directly -- hence a list that simply leaves src/fs/ out. Add a
              // line here when src/ grows a new top-level directory.
              //
              // src/fs/ is left off deliberately. src/fs/fs.ts is the contract
              // both halves implement -- the FS interface, the FileEntry
              // shape, and the predicates deciding what counts as a user's own
              // file -- and sharing it is what stops the two backends drifting
              // on questions like what "hidden" means. Its neighbours
              // (opfs.ts, node.ts) are not lint-blocked, but importing one
              // fails the server's typecheck, whose tsconfig omits the DOM lib
              // that those files depend on. That is an error, not a warning.
              group: [
                '**/src/app/*',
                '**/src/js/*',
                '**/src/lib/*',
                '**/src/lpm/*',
                '**/src/prettier/*',
                '**/src/scheme/*',
                '**/src/theme/*',
                '**/src/scamper',
                '**/src/utils',
              ],
              allowTypeImports: true,
              message:
                'The server may import *types* from src/ (use `import type`), but values only from src/fs/fs.ts -- the rest of src/ is browser code.',
            },
          ],
        },
      ],
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
      semi: ['warn', 'never'],
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
