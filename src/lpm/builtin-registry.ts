import { Module } from './lang.js'

// N.B., a neutral home for the builtin-library registry, deliberately kept
// dependency-free (besides the Module type). fiber.ts needs this for its own
// loadModule(), and src/lib/index.ts populates it by compiling and running
// the builtin libraries' Scamper source (which itself runs through a Fiber)
// -- routing both sides through src/lib/index.ts directly would make fiber.ts
// and src/lib/index.ts each wait on the other to finish evaluating.
export const builtinLibs = new Map<string, Module>()
