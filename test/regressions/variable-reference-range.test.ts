import { afterEach, describe, expect, test, vi } from 'vitest'
import * as fs from '../../src/fs'
import { runProgram } from '../harness.js'

// https://github.com/slag-plt/scamper/issues/753
//
// The other half of #753 (see if-guard-range.test.ts for the `if` guard):
// `VarHandler`'s two errors were raised with no range at all, so a misspelled
// name -- the most common mistake a student makes -- arrived unlocated:
//
//   Runtime error: Variable not found: nosuchvar
//
// The scope check does warn about an unbound name before the program runs
// (src/scheme/scope.ts:135-144 pushes `Undefined variable '<name>'` at the
// reference's own range, and the IDE squiggles it), so this error is not the
// student's only signal. It is the only one that says which reference the run
// actually *reached* -- picking out one of two identical typos -- and its range
// is what files the error under a notebook cell.

/** Backs a run's file imports with `files`, by name (as #557's test does). */
function mockFS(files: Record<string, string>): void {
  vi.spyOn(fs, 'getFS').mockReturnValue({
    fileExists: (f: string) => Promise.resolve(f in files),
    loadFile: (f: string) => Promise.resolve(files[f]),
  } as unknown as ReturnType<typeof fs.getFS>)
}

afterEach(() => {
  vi.restoreAllMocks()
})

describe('an unresolved variable reports where it is (#753)', () => {
  test('the range covers the name alone, not the form holding it', async () => {
    expect(await runProgram('(+ 1 nosuchvar)')).toEqual([
      'Runtime error [1:6-1:14]: Variable not found: nosuchvar',
    ])
  })

  test('a bare reference reports itself', async () => {
    expect(await runProgram('nosuchvar')).toEqual([
      'Runtime error [1:1-1:9]: Variable not found: nosuchvar',
    ])
  })

  // The range picks out which reference failed, which is the whole point of
  // locating this error: a file naming the same typo twice underlines the one
  // that ran.
  test('the reported reference is the one that ran', async () => {
    expect(
      await runProgram(`(define ok 1)
(+ ok nosuchvar)`),
    ).toEqual(['Runtime error [2:7-2:15]: Variable not found: nosuchvar'])
  })

  // As with `let` and `if`, the frame's `origin` decides this and its `hidden`
  // flag does not: `map` hides the student's lambda from the trace but leaves
  // its origin 'user', so the name inside it is still theirs to fix.
  test("a reference inside a library call reports the student's own name", async () => {
    expect(
      await runProgram('(map (lambda (x) nosuchvar) (list 1))'),
    ).toEqual(['Runtime error [1:18-1:26]: Variable not found: nosuchvar'])
  })

  // An imported file is 'import', not 'builtin', so it keeps its own range and
  // names its own file -- the #557 contract. A consumer that cannot place a
  // foreign range says the file in words instead of pointing into the open
  // document (see ScamperError.modName).
  test('a name missing in an imported file names that file, not the main one', async () => {
    mockFS({
      'm.scm': `(define-export twice
  (lambda (n) (* n nosuchvar)))`,
    })
    expect(
      await runProgram(`(import "m.scm")
(twice 2)`),
    ).toEqual(['Runtime error [m.scm 2:20-2:28]: Variable not found: nosuchvar'])
  })
})

describe('a variable used before it is defined reports where it is (#753)', () => {
  test('the range covers the reference, not the binding around it', async () => {
    expect(await runProgram('(let ([x x]) x)')).toEqual([
      'Runtime error [1:10-1:10]: Variable "x" is referenced before it is defined',
    ])
  })

  // Kept beside the case above for the contrast, which is the informative
  // part: a `let` is letrec, so every binder is declared as a HOLE before any
  // value is evaluated and a forward reference finds one -- the error above. A
  // top-level `define` is not pre-declared, so the same mistake between two
  // statements does not reach the HOLE arm at all; it is an ordinary unresolved
  // name, with the *other* message. Both are now located at the reference.
  test('a top-level forward reference is not found, rather than held', async () => {
    expect(
      await runProgram(`(define a b)
(define b 1)`),
    ).toEqual([
      'Runtime error [1:11-1:11]: Variable not found: b',
    ])
  })
})

// N.B., no case for the builtin arm of either error, because a library cannot
// reach it. Two things close it off:
//
//   - A library source cannot name anything outside {its own top level} u
//     prelude u runtime and still load. `scopeCheckProgram`
//     (src/scheme/scope.ts:702-742) seeds globals with the runtime and prelude
//     symbols only and warns on any undefined variable, and loadLibrary
//     (src/lib/index.ts:33-39) turns *any* diagnostic into an ICE.
//   - A builtin closure's free names cannot be captured by the student either:
//     `extendWithImport` (src/lpm/lang.ts:255-266) always re-homes, so they
//     resolve against the module's own bindings over an env holding runtime and
//     prelude with an empty top level. That re-homing is there precisely
//     because a user's `(define car 5)` used to capture the prelude's internal
//     calls to `car`.
//
// The fallback stays, for two reasons rather than as dead defence. At library
// load time a frame's `callRange` is `Range.none`, whose `begin.idx` is -1, so
// `ScamperError.toString` prints no location and `isPlaceable` refuses to
// place it: the fallback degrades to the old unlocated behaviour instead of
// inventing `[1:1-1:1]`. And a name in *pattern* position escapes the scope
// check across libraries -- gradescope.scm:107-113 names test.scm's
// constructors -- so were such a name ever to move into value position, the
// builtin arm would fire and correctly blame the student's call.
