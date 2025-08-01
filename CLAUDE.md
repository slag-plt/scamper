# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands
- `npm install` - Install NPM dependencies
- `npm run dev` - Start local development server with hot reloading
- `npm run build` - Build the project (TypeScript compilation + Vite build)
- `npm run test` - Run Jest tests
- `npm run deploy` - Deploy Scamper to production server
- `npm run clean` - Remove dist directory
- `npm run preview` - Preview the built application

## Project Architecture
Scamper is a mini-Scheme implementation for teaching multimedia programming on the web. The architecture consists of:

### Core Runtime Components
- **Parser** (`src/parser.ts`) - Parses Scamper source code into AST
- **Semantics** (`src/sem.ts`) - Core evaluation engine and runtime
- **Language definitions** (`src/lang.ts`) - Core language types and structures
- **LPM (Local Parameter Machine)** (`src/lpm/`) - Low-level virtual machine implementation
  - `runtime.ts` - Runtime execution engine
  - `machine.ts` - Virtual machine state and operations
  - `ops.ts` - Operation definitions

### Library System
- **Built-in libraries** (`src/lib/`) - Core Scheme libraries implemented in TypeScript
  - `builtin.ts` - Registry mapping library names to implementations
  - `prelude.ts` - Standard library functions always available
  - Domain-specific libraries: `image.ts`, `canvas.ts`, `audio.ts`, `music.ts`, `html.ts`, `reactive.ts`, `test.ts`, `lab.ts`
- **Library architecture** - Each library exports a mapping of `[string, any][]` where strings are Scamper identifiers

### Web Integration
- **IDE** (`src/web/ide.ts`) - Web-based development environment
- **Runner** (`src/web/runner.ts`) - Code execution interface
- **File system** (`src/web/fs/`) - Virtual file system for web environment
- **CodeMirror integration** (`src/codemirror/`) - Syntax highlighting and editing

### Type System
Scamper-to-JavaScript type mapping:
- Primitives: boolean, number, string, null (void), Array (vector)
- Tagged objects: `{ _scamperTag: 'pair'|'struct'|'closure'|'jsfunc' }`
- Functions can be Scamper closures, wrapped JS functions, or raw JS functions

## Testing
- Tests are located in `test/` directory
- Jest configuration in `jest.config.ts` with TypeScript support
- Test files follow `*.test.ts` pattern
- Uses jsdom environment for DOM testing

## Build System
- TypeScript with ESNext target
- Vite for bundling and development server
- ESLint for linting (standard TypeScript configuration)
- Source maps and declarations enabled for debugging

## Library Development
When adding new libraries:
1. Create library file in `src/lib/`
2. Export library mapping as `[string, any][]`
3. Add entry to `builtinLibs` map in `src/lib/builtin.ts`
4. Use `mkJsFunction` from `sem.ts` to wrap JavaScript functions with arity checking
5. Follow existing patterns in other library files

## Development Notes
- The project uses ES modules throughout
- Strict TypeScript configuration with comprehensive linting
- Libraries should throw `ScamperError` for runtime errors and `ICE` for internal compiler errors.
- Use `callFunction` from `sem.ts` to invoke higher-order functions safely