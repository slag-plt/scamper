# CLAUDE.md

This file provides guidance to LLM agents when working with code in this repository.

## Project Overview

**Scamper** is a mini-Scheme implementation designed for teaching multimedia programming on the web. It provides a complete web-based programming environment with IDE, documentation viewer, and various runner interfaces.

## Development Commands

### Essential Commands
- `npm install` - Install dependencies  
- `npm run dev` - Start Vite development server for local development
- `npm run build` - Full production build (TypeScript compilation + Vite build)
- `npm test` - Run all Jest tests
- `npm run deploy` - Deploy to production server (requires Mac/Linux and compsci host)
- `npm run clean` - Remove dist directory

### Testing
- Tests are located in `/test/` directory with `.test.ts` extension
- Jest runs in jsdom environment with TypeScript support
- Key test files: `libs.test.ts`, `prelude.test.ts`, `runtime.test.ts`, `lpm/machine.test.ts`
- Individual test files can be run with: `npm test -- <filename>`

## Architecture Overview

### Core Language Pipeline
1. **Lexing/Reading** (`src/scheme/reader.ts`) - S-expression parsing
2. **AST Parsing** (`src/scheme/ast.ts`) - Convert to abstract syntax tree
3. **Macro Expansion** (`src/scheme/expansion.ts`) - Expand Scheme macros
4. **Scope Checking** (`src/scheme/scope.ts`) - Variable resolution
5. **Code Generation** (`src/scheme/codegen.ts`) - Lower to LPM bytecode
6. **Execution** (`src/lpm/`) - Language Processing Machine runtime

### Key Source Directories
- `src/scheme/` - Scheme language implementation (parser, AST, codegen, etc.)
- `src/lpm/` - LPM runtime execution engine
- `src/lib/` - Built-in libraries (multimedia, audio, canvas, image, reactive, etc.)
- `src/web/` - Web interface components (IDE, file system, runners)
- `src/codemirror/` - Code editor integration with Scheme syntax highlighting
- `src/docs/` - Documentation generation system

### Main Classes
- `Scamper` class in `scamper.ts` - Main orchestrator for compilation and execution

### Web Interfaces (Multiple Entry Points)
- `ide.html` - Full IDE with split-pane editor and output
- `runner.html` - Simple program runner  
- `docs.html` - Documentation viewer
- `web.html` - Alternative web interface
- `index.html` - File chooser/main entry point

## Library Development

Built-in libraries are in `/src/lib/`. Each library exports a mapping of `[string, any][]` from Scamper identifiers to values/functions.

### Type Mappings
- JavaScript primitives map directly: `boolean`, `number`, `string`, `null` (null), `undefined` (void), `Array` (vector)
- Scamper objects use `##scamperTag##` fields: pairs, lists, structs, closures, wrapped JS functions

### Key Utilities
- `ScamperError` in `lang.ts` - Primary exception type for libraries
- `callScamperFn()` in `lpm` - Invoke higher-order functions
- Library registration in `builtin.ts` for import statements

## Build Configuration

- **Vite** (main build tool) with TypeScript compilation
- **Multi-entry build** creates versioned assets for each interface
- **ES2023 target** with strict TypeScript configuration
- **Source maps** generated for both JS and TS
- Output: `dist/` for web assets, `types/` for TypeScript declarations

## Current Development

- Active branch: `lpm` (Little Pattern Machine development)
- Version: 3.0.0
- Key changes appear to be in the LPM runtime system