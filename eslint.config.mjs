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
      // Vendored WebAudioFont, carried as-is and opening with `@ts-nocheck`.
      // The pattern used to read `src/js/webaudiofont/*` and so matched
      // nothing: the file moved under `music/` in fe89945 and the ignore did
      // not follow, leaving 622 warnings from code we do not maintain. The
      // wrapper beside it, `webaudiofont.ts`, is ours and stays linted.
      'src/js/music/webaudiofont/WebAudioFontPlayer.ts',
      'src/scheme/generated/*',
      'src/lib/generated/*',
    ]),
  ],
  {
    languageOptions: {
      parserOptions: {
        projectService: {
          // Only the config files at the repository root land here.
          // Everything in scripts/ is covered by scripts/tsconfig.json --
          // this list is capped at eight entries, and silently breaks
          // linting for every file in it once exceeded.
          allowDefaultProject: ['eslint.config.mjs', 'vite.config.ts'],
          // Which tsconfig those files are typed against. Without this they
          // fall to an inferred project with no `allowJs`, so every `.mjs`
          // plugin import resolved to nothing and each plugin call read as an
          // unsafe call of an unresolved type (#154).
          defaultProject: 'scripts/tsconfig.json',
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
      'scripts/vite-plugin-dev-server-config.mjs',
      'scripts/dev-memory.mjs',
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
              // Anchored on the workspace's source directory, not on the
              // bare name: these globs match a path segment anywhere (as
              // .gitignore does), so `**/server` would also catch the client's
              // own src/fs/server.ts and src/history/server.ts.
              group: ['**/server/src'],
              message:
                'The client must not import server code. The two share only the contracts in src/fs/fs.ts and src/history/policy.ts, which the server imports from here -- not the other way around.',
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
              // Everything below concerns *value* imports.
              //
              // Spelled as a list of what is forbidden rather than "all of src/
              // except the shared bits", because these globs follow .gitignore
              // semantics: a pattern matches a path segment anywhere and
              // everything beneath it, and the `!` negation this rule accepts
              // does not re-admit a descendant. So the exceptions are expressed
              // by leaving them off. Add a line when src/ grows a top-level
              // directory the server should not reach into.
              //
              // Two modules are omitted deliberately -- the only ones the
              // server may take values from:
              //
              //   src/fs/fs.ts        the FS interface, the FileEntry shape,
              //                       and what counts as a user's own file
              //   src/history/policy  when a save is worth recording
              //
              // Sharing them is what stops the two halves drifting on questions
              // like what "hidden" means or how long the merge window is. Both
              // are pure: no DOM, no fetch, no storage.
              //
              // Their neighbours in src/fs/ are not lint-blocked, but importing
              // one fails the server's typecheck, whose tsconfig omits the DOM
              // lib they depend on. That is an error, not a warning.
              group: [
                '**/src/app/*',
                '**/src/js/*',
                '**/src/lib/*',
                '**/src/lpm/*',
                '**/src/scheme/*',
                '**/src/theme/*',
                '**/src/history/flat-file',
                '**/src/history/history',
                '**/src/history/index',
                '**/src/history/none',
                '**/src/history/server',
                '**/src/scamper',
                '**/src/utils',
              ],
              allowTypeImports: true,
              message:
                'The server may import *types* from src/ (use `import type`), but values only from src/fs/fs.ts and src/history/policy.ts -- the rest of src/ is browser code.',
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
      // A number in a template string is ordinary JavaScript, and reads far
      // better than `${n.toString()}` -- `rgb(${r} ${g} ${b})` being the case
      // that settled it. This is typescript-eslint's own default for the rule;
      // strictTypeChecked is what turns it off (#154).
      //
      // N.B. `allowAny` is restated because naming the rule again resets every
      // option it does not mention, and strictTypeChecked's `allowAny: false`
      // is worth keeping: an `any` in a template is a hole, where a number is
      // just a number.
      '@typescript-eslint/restrict-template-expressions': [
        'warn',
        { allowNumber: true, allowAny: false },
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
