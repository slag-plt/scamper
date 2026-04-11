import eslint from "@eslint/js"
import { defineConfig, globalIgnores } from "eslint/config"
import tseslint from "typescript-eslint"
import globals from "globals"
import eslintConfigPrettier from "eslint-config-prettier/flat"
import "eslint-plugin-only-warn"
import vueEslint from "eslint-plugin-vue"

export default defineConfig(
  eslint.configs.recommended,
  tseslint.configs.strictTypeChecked,
  tseslint.configs.stylisticTypeChecked,
  ...vueEslint.configs["flat/recommended-error"],
  {
    files: ["*.vue", "**/*.vue"],
    languageOptions: {
      parserOptions: {
        parser: tseslint.parser,
      },
      globals: {
        ...globals.browser,
      },
    },
  },
  eslintConfigPrettier,
  [globalIgnores(["dist/*", "types/*", "src/lib/webaudiofont/*"])],
  {
    languageOptions: {
      parserOptions: {
        projectService: {
          allowDefaultProject: ["eslint.config.mjs", "vite.config.ts"],
        },
        tsconfigRootDir: import.meta.dirname,
        extraFileExtensions: [".vue"],
      },
    },
  },
  {
    files: ["vite.config.ts"],
    languageOptions: {
      globals: {
        ...globals.node,
      },
    },
  },
)
