import { describe, test, expect, vi, afterEach } from 'vitest'
import * as fs from '../../src/fs'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { makeScopeTreeFromProgram, ScopeTree } from '../../src/scheme/scope-tree'
import { expandProgram } from '../../src/scheme/expansion'
import { parseProgramFromSource } from '../../src/scheme/lezer-bridge'
import * as A from '../../src/scheme/ast'
import { Loc, Range } from '../../src/lpm'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import { runProgram } from '../harness.js'

// This suite exercises scope checking (`scopeCheckProgram`) and scope-tree
// construction (`makeScopeTreeFromProgram`) across every binding form and a
// range of interesting cases (shadowing, let vs let* visibility, imports, ...).
// The symbol DB is initialized globally by test/setup.ts.

///// Helpers //////////////////////////////////////////////////////////////////

function parse(src: string): A.Prog {
  const errs: ScamperDiagnostic[] = []
  const prog = parseProgramFromSource(errs, src)
  expect(
    errs.map((e) => e.message),
    'test source should itself parse cleanly',
  ).toEqual([])
  return prog
}

// Scope-checks `src` (expanded, as the real pipeline does) and returns the
// resulting diagnostic messages.
async function scopeErrors(src: string): Promise<string[]> {
  const errors: ScamperDiagnostic[] = []
  await scopeCheckProgram(errors, expandProgram(parse(src)))
  return errors.map((e) => e.message)
}

// Installs a mock file system exposing `files` (filename -> source); any other
// filename is reported as non-existent.
function mockFS(files: Record<string, string>): void {
  vi.spyOn(fs, 'getFS').mockReturnValue({
    fileExists: (f: string) => Promise.resolve(f in files),
    loadFile: (f: string) =>
      f in files
        ? Promise.resolve(files[f])
        : Promise.reject(new Error(`no such file: ${f}`)),
  } as unknown as ReturnType<typeof fs.getFS>)
}

afterEach(() => {
  vi.restoreAllMocks()
})

// ---- scope-tree query helpers ----

function isDelim(ch: string | undefined): boolean {
  return ch === undefined || /[\s()[\]'"]/.test(ch)
}

// Index of the `n`-th whole-token occurrence of `token` (delimited on both
// sides), so a search for `x` doesn't match the `x` inside a longer name.
function nthTokenIndex(s: string, token: string, n: number): number {
  let from = 0
  let count = 0
  for (;;) {
    const idx = s.indexOf(token, from)
    if (idx < 0) return -1
    const before = idx === 0 ? undefined : s[idx - 1]
    const after = idx + token.length >= s.length ? undefined : s[idx + token.length]
    if (isDelim(before) && isDelim(after)) {
      if (count === n) return idx
      count++
    }
    from = idx + 1
  }
}

function locAt(src: string, idx: number): Loc {
  const before = src.slice(0, idx)
  return new Loc(
    before.split('\n').length,
    idx - (before.lastIndexOf('\n') + 1) + 1,
    idx,
  )
}

// The source range of the `occurrence`-th whole-token appearance of `token`.
function rangeOf(src: string, token: string, occurrence = 0): Range {
  const start = nthTokenIndex(src, token, occurrence)
  expect(
    start,
    `token ${JSON.stringify(token)} #${occurrence.toString()} not found`,
  ).toBeGreaterThanOrEqual(0)
  return new Range(locAt(src, start), locAt(src, start + token.length - 1))
}

async function treeOf(src: string): Promise<ScopeTree> {
  return makeScopeTreeFromProgram(parse(src))
}

// The identifiers visible at the `token` position, local-to-global.
async function visibleAt(
  src: string,
  token: string,
  occurrence = 0,
): Promise<string[]> {
  const scope = (await treeOf(src)).getInnermostScope(
    rangeOf(src, token, occurrence),
  )
  expect(scope, 'expected an enclosing scope at that position').toBeDefined()
  return scope === undefined
    ? []
    : scope.getVisibleIdentifiers().map((i) => i.name)
}

///// Scope checking ///////////////////////////////////////////////////////////

describe('scope checking', () => {
  describe('well-scoped programs report no errors', () => {
    test.each([
      ['empty program', ''],
      ['number literal', '42'],
      ['string literal', '"hello"'],
      ['boolean literal', '#t'],
      ['char literal', '#\\a'],
      ['null literal', 'null'],
      ['prelude application', '(+ 1 2)'],
      ['define then reference', '(define x 1)\nx'],
      ['define using an earlier define', '(define x 1)\n(define y (+ x 1))'],
      ['self-recursive define', '(define f (lambda (n) (f n)))'],
      ['lambda parameter', '(lambda (x) x)'],
      ['lambda with several parameters', '(lambda (x y z) (+ x (+ y z)))'],
      ['lambda with a rest parameter', '(lambda (x & rest) rest)'],
      ['nullary lambda', '(lambda () 0)'],
      ['let binding', '(let ([x 1]) x)'],
      ['let with independent bindings', '(let ([x 1] [y 2]) (+ x y))'],
      ['nested let shadowing', '(let ([x 1]) (let ([x 2]) x))'],
      ['let telescoping (a later binding sees an earlier one)', '(let ([x 1] [y x]) y)'],
      ['and / or', '(and #t (or #f #t))'],
      ['cond', '(cond [#t 1] [#f 2])'],
      ['if', '(if #t 1 2)'],
      ['begin', '(begin 1 2 3)'],
      ['match variable pattern', '(match 5 [x x])'],
      ['match wildcard pattern', '(match 5 [_ 0])'],
      ['match literal pattern', '(match 5 [5 "five"] [_ "?"])'],
      ['match constructor pattern', '(match (cons 1 2) [(cons a b) (+ a b)])'],
      [
        'nested constructor pattern',
        '(match (cons 1 (cons 2 3)) [(cons a (cons b c)) (+ a (+ b c))])',
      ],
      ['quoted data is not scope-checked', "'(these are not variables)"],
      ['quote form is not scope-checked', '(quote (foo bar baz))'],
      ['error form', '(error "boom")'],
      ['apply form', '(apply + (list 1 2))'],
      ['js-var is a bound global (its arg is a literal)', '(js-var "Math")'],
      ['display', '(define x 1)\n(display x)'],
      ['struct definition', '(struct posn (x y))'],
      [
        'struct constructor/predicate/accessors are bound',
        '(struct posn (x y))\n(posn? (posn 1 2))\n(posn-x (posn 1 2))',
      ],
    ])('%s', async (_name, src) => {
      expect(await scopeErrors(src)).toEqual([])
    })
  })

  describe('undefined variables', () => {
    test('a bare reference', async () => {
      expect(await scopeErrors('foo')).toEqual(["Undefined variable 'foo'"])
    })
    test('the head of an application', async () => {
      expect(await scopeErrors('(nope 1 2)')).toEqual([
        "Undefined variable 'nope'",
      ])
    })
    test('an argument of an application', async () => {
      expect(await scopeErrors('(+ 1 bar)')).toEqual([
        "Undefined variable 'bar'",
      ])
    })
    test('inside a lambda body', async () => {
      expect(await scopeErrors('(lambda (x) y)')).toEqual([
        "Undefined variable 'y'",
      ])
    })
    test('a lambda parameter is not visible outside the lambda', async () => {
      expect(await scopeErrors('(define f (lambda (x) x))\nx')).toEqual([
        "Undefined variable 'x'",
      ])
    })
    test('a let binding is not visible outside the let', async () => {
      expect(await scopeErrors('(let ([x 1]) 0)\nx')).toEqual([
        "Undefined variable 'x'",
      ])
    })
    test('letrec: a binder is in scope in sibling binding values (not a scope error)', async () => {
      // A forward reference to a not-yet-evaluated binding is a *runtime* error,
      // not a scope error -- the name is genuinely in scope.
      expect(await scopeErrors('(let ([a b] [b 1]) a)')).toEqual([])
    })
    test('inside an if branch', async () => {
      expect(await scopeErrors('(if #t x 2)')).toEqual([
        "Undefined variable 'x'",
      ])
    })
    test('inside a match scrutinee', async () => {
      expect(await scopeErrors('(match x [_ 0])')).toEqual([
        "Undefined variable 'x'",
      ])
    })
    test('inside a match branch body', async () => {
      expect(await scopeErrors('(match 5 [x y])')).toEqual([
        "Undefined variable 'y'",
      ])
    })
    test('a match pattern variable is not visible in a sibling branch', async () => {
      expect(
        await scopeErrors('(match (cons 1 2) [(cons a b) 0] [_ a])'),
      ).toEqual(["Undefined variable 'a'"])
    })
  })

  describe('top-level definitions are mutually recursive', () => {
    // Top-level bindings share one mutually-recursive scope (Racket module
    // semantics), so their order in the program does not affect visibility.
    test('a forward reference to a later define resolves', async () => {
      expect(await scopeErrors('x\n(define x 1)')).toEqual([])
    })
    test('mutual recursion between top-level defines is allowed', async () => {
      expect(
        await scopeErrors(
          '(define f (lambda (n) (g n)))\n(define g (lambda (n) (f n)))',
        ),
      ).toEqual([])
    })
    test('a define may reference a name defined later', async () => {
      expect(
        await scopeErrors('(define a (lambda () (b)))\n(define b (lambda () 1))'),
      ).toEqual([])
    })
    test('a genuinely undefined name is still flagged', async () => {
      expect(await scopeErrors('(define f (lambda (n) (nope n)))')).toEqual([
        "Undefined variable 'nope'",
      ])
    })
  })

  describe('duplicate bindings', () => {
    test('duplicate lambda parameters', async () => {
      expect(await scopeErrors('(lambda (x x) x)')).toContain(
        "Duplicate variable 'x' encountered in binding list",
      )
    })
    test('a parameter colliding with the rest parameter', async () => {
      expect(await scopeErrors('(lambda (x & x) x)')).toContain(
        "Duplicate variable 'x' encountered in binding list",
      )
    })
    test('duplicate let bindings are an error', async () => {
      expect(await scopeErrors('(let ([x 1] [x 2]) x)')).toContain(
        "Duplicate binding 'x' in let",
      )
    })
    test('duplicate match pattern variables', async () => {
      expect(
        await scopeErrors('(match (cons 1 2) [(cons x x) x])'),
      ).toContain("Duplicate binding variable 'x' encountered in pattern")
    })
    test('redefining a global', async () => {
      expect(await scopeErrors('(define x 1)\n(define x 2)')).toEqual([
        "Global variable 'x' is already defined",
      ])
    })
    test('redefining a prelude binding', async () => {
      expect(await scopeErrors('(define map 1)')).toEqual([
        "Global variable 'map' is already defined",
      ])
    })
  })

  describe('local shadowing is allowed across scopes', () => {
    test.each([
      ['a parameter shadows a global', '(define x 1)\n((lambda (x) x) 2)'],
      ['a let binding shadows a global', '(define x 1)\n(let ([x 2]) x)'],
      ['a let binding shadows a parameter', '((lambda (x) (let ([x 2]) x)) 1)'],
      [
        'a nested parameter shadows an outer parameter',
        '((lambda (x) ((lambda (x) x) 2)) 1)',
      ],
      [
        'a match pattern shadows a parameter',
        '((lambda (x) (match 5 [x x])) 1)',
      ],
      ['a match pattern shadows a let binding', '(let ([x 1]) (match 5 [x x]))'],
    ])('%s', async (_name, src) => {
      expect(await scopeErrors(src)).toEqual([])
    })
  })

  describe('imports', () => {
    test('a known builtin library imports cleanly', async () => {
      expect(await scopeErrors('(import image)')).toEqual([])
    })
    test('an unknown builtin library is reported', async () => {
      expect(await scopeErrors('(import not-a-real-lib)')).toEqual([
        "No such built-in library: 'not-a-real-lib'",
      ])
    })
    test("a file import's definitions become visible", async () => {
      mockFS({ 'utils.scm': '(define helper 1)' })
      expect(await scopeErrors('(import "utils.scm")\nhelper')).toEqual([])
    })
    test('only names the module actually defines are imported', async () => {
      mockFS({ 'utils.scm': '(define helper 1)' })
      expect(await scopeErrors('(import "utils.scm")\nmissing')).toEqual([
        "Undefined variable 'missing'",
      ])
    })
    test('a missing file is reported as a diagnostic', async () => {
      mockFS({})
      expect(await scopeErrors('(import "gone.scm")')).toEqual([
        "File 'gone.scm' does not exist",
      ])
    })
    test('an unparseable file is reported as a diagnostic', async () => {
      mockFS({ 'bad.scm': '(1 2' })
      expect(await scopeErrors('(import "bad.scm")')).toEqual([
        "Could not load module 'bad.scm'",
      ])
    })
    test('multiple import failures are all collected', async () => {
      mockFS({ 'bad.scm': '(1 2' })
      expect(
        await scopeErrors('(import "gone.scm")\n(import "bad.scm")'),
      ).toEqual([
        "File 'gone.scm' does not exist",
        "Could not load module 'bad.scm'",
      ])
    })
    test('transitively-imported names are not directly visible', async () => {
      mockFS({
        'a.scm': '(import "b.scm")\n(define fromA 1)',
        'b.scm': '(define fromB 2)',
      })
      expect(await scopeErrors('(import "a.scm")\nfromA')).toEqual([])
      expect(await scopeErrors('(import "a.scm")\nfromB')).toEqual([
        "Undefined variable 'fromB'",
      ])
    })
    test('imports resolve through several layers, exposing only direct names', async () => {
      mockFS({
        'a.scm': '(import "b.scm")\n(define fromA 1)',
        'b.scm': '(import "c.scm")\n(define fromB 2)',
        'c.scm': '(define fromC 3)',
      })
      expect(await scopeErrors('(import "a.scm")\nfromA')).toEqual([])
      expect(await scopeErrors('(import "a.scm")\nfromB')).toEqual([
        "Undefined variable 'fromB'",
      ])
      expect(await scopeErrors('(import "a.scm")\nfromC')).toEqual([
        "Undefined variable 'fromC'",
      ])
    })
    test('defining a name already brought in by an import is disallowed', async () => {
      mockFS({ 'utils.scm': '(define helper 1)' })
      expect(
        await scopeErrors('(import "utils.scm")\n(define helper 2)'),
      ).toEqual(["Global variable 'helper' is already defined"])
    })
    test('an import after a same-named define is also flagged (symmetric)', async () => {
      // A define/import name clash is an error regardless of order (Racket: an
      // identifier can be either imported or defined, but not both).
      mockFS({ 'utils.scm': '(define helper 1)' })
      expect(
        await scopeErrors('(define helper 1)\n(import "utils.scm")'),
      ).toEqual(["Global variable 'helper' is already defined"])
    })
    test('two imports exporting the same name collide', async () => {
      mockFS({ 'x.scm': '(define dup 1)', 'y.scm': '(define dup 2)' })
      expect(await scopeErrors('(import "x.scm")\n(import "y.scm")')).toEqual([
        "Global variable 'dup' is already defined",
      ])
    })
    test('re-importing the same module is idempotent (no collision)', async () => {
      mockFS({ 'utils.scm': '(define helper 1)' })
      expect(
        await scopeErrors('(import "utils.scm")\n(import "utils.scm")\nhelper'),
      ).toEqual([])
    })
    test('a local binding may shadow an imported name', async () => {
      mockFS({ 'utils.scm': '(define helper 1)' })
      expect(
        await scopeErrors('(import "utils.scm")\n((lambda (helper) helper) 5)'),
      ).toEqual([])
    })
    test('a failure in a deep transitive import is surfaced by the importer', async () => {
      // N.B., the program imports only a.scm (which parses); the broken c.scm
      // it transitively pulls in is reported, pointing at the top-level import
      // that pulled it in.
      mockFS({ 'a.scm': '(import "c.scm")\n(define fromA 1)', 'c.scm': '(1 2' })
      expect(await scopeErrors('(import "a.scm")\nfromA')).toEqual([
        "Could not load module 'c.scm' (imported by 'a.scm')",
      ])
    })
  })

  describe('docstrings', () => {
    const withDoc = (sig: string, param: string, body: string) =>
      `;;; ${sig}\n;;;  ${param}\n;;; A description.\n${body}`

    test('a well-formed docstring produces no errors', async () => {
      expect(
        await scopeErrors(
          withDoc(
            '(add1 n) -> number?',
            'n : number?',
            '(define add1 (lambda (n) (+ n 1)))',
          ),
        ),
      ).toEqual([])
    })
    test('a docstring name that mismatches the definition', async () => {
      expect(
        await scopeErrors(
          withDoc(
            '(wrong n) -> number?',
            'n : number?',
            '(define add1 (lambda (n) (+ n 1)))',
          ),
        ),
      ).toContain(
        'Docstring function name "wrong" does not match defined name "add1"',
      )
    })
    test('an undefined predicate in a docstring', async () => {
      expect(
        await scopeErrors(
          withDoc(
            '(add1 n) -> bogus?',
            'n : number?',
            '(define add1 (lambda (n) (+ n 1)))',
          ),
        ),
      ).toContain('Undefined predicate "bogus?"')
    })
  })
})

///// Scope tree ///////////////////////////////////////////////////////////////

describe('scope tree', () => {
  describe('global scope', () => {
    test('an empty program still exposes the standard library', async () => {
      const names = (await treeOf('')).getVisibleIdentifiers().map((i) => i.name)
      expect(names).toContain('+')
      expect(names).toContain('map')
    })
    test('top-level definitions are visible globally', async () => {
      const names = (await treeOf('(define x 1)\n(define y 2)'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(names).toEqual(expect.arrayContaining(['x', 'y', '+']))
    })
    test('a struct binds its constructor name', async () => {
      const names = (await treeOf('(struct posn (x y))'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(names).toContain('posn')
      // N.B., the derived `posn?`/`posn-x`/`posn-y` bindings are only
      // synthesized during expansion, so the surface scope tree omits them.
      expect(names).not.toContain('posn?')
    })
  })

  describe('nested scopes and visibility', () => {
    test('lambda parameters are visible in the body, innermost first', async () => {
      const src = '(define f (lambda (a b) (+ a b)))'
      const names = await visibleAt(src, 'a', 1) // the `a` in the body
      expect(names.slice(0, 2)).toEqual(['a', 'b'])
      expect(names).toEqual(expect.arrayContaining(['a', 'b', 'f', '+']))
    })
    test('a rest parameter is visible in the body', async () => {
      const names = await visibleAt(
        '(define f (lambda (x & rest) rest))',
        'rest',
        1,
      )
      expect(names).toEqual(expect.arrayContaining(['x', 'rest']))
    })
    test('letrec: let binders are visible in binding values and in the body', async () => {
      const src = '(define f (let ([c 5] [d c]) d))'
      // in the body, both bindings are visible
      expect(await visibleAt(src, 'd', 1)).toEqual(
        expect.arrayContaining(['c', 'd']),
      )
      // at the `c` reference inside `[d c]` (a binding value), the let's own
      // bindings are also visible (letrec)
      expect(await visibleAt(src, 'c', 1)).toEqual(
        expect.arrayContaining(['c', 'd']),
      )
    })
    test('match pattern variables are visible only in their branch body', async () => {
      const src = '(define f (lambda (p) (match p [(cons a b) (+ a b)])))'
      // in the branch body
      expect(await visibleAt(src, 'a', 1)).toEqual(
        expect.arrayContaining(['a', 'b', 'p', 'f']),
      )
      // at the scrutinee `p`, the pattern variables are not yet in scope
      const atScrutinee = await visibleAt(src, 'p', 1)
      expect(atScrutinee).toContain('p')
      expect(atScrutinee).not.toContain('a')
    })
    test('deeply nested scopes resolve local-to-global', async () => {
      const src =
        '(define g 0)\n(define f (lambda (a) (let ([b 1]) (match b [c (+ a (+ b c))]))))'
      const names = await visibleAt(src, 'c', 1) // the `c` in the branch body
      expect(names.slice(0, 3)).toEqual(['c', 'b', 'a'])
      expect(names).toEqual(expect.arrayContaining(['g', 'f']))
    })
    test('a local binding shadows an outer one, appearing first', async () => {
      const src = '(define x 1)\n(define f (lambda (x) x))'
      const names = await visibleAt(src, 'x', 2) // the `x` in the lambda body
      expect(names[0]).toBe('x') // the parameter, innermost
      // the global `x` is still present but shadowed by the parameter
      expect(names.filter((n) => n === 'x')).toHaveLength(2)
    })
  })

  describe('getInnermostScope', () => {
    test('a top-level position resolves to the global scope', async () => {
      const src = '(define x 1)\n(+ x 1)'
      const names = await visibleAt(src, '+', 0)
      expect(names).toContain('x')
    })
    test('a position outside the program has no enclosing scope', async () => {
      const src = '(define x 1)'
      const tree = await treeOf(src)
      const past = new Range(
        locAt(src, src.length + 5),
        locAt(src, src.length + 10),
      )
      expect(tree.getInnermostScope(past)).toBeUndefined()
    })
  })

  describe('imports', () => {
    test("a file import's definitions are visible globally", async () => {
      mockFS({ 'utils.scm': '(define helper 1)\n(define other 2)' })
      const names = (await treeOf('(import "utils.scm")'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(names).toEqual(expect.arrayContaining(['helper', 'other']))
    })
    test('only directly-imported names are visible, not transitive ones', async () => {
      mockFS({
        'a.scm': '(import "b.scm")\n(define fromA 1)',
        'b.scm': '(define fromB 2)',
      })
      const names = (await treeOf('(import "a.scm")'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(names).toContain('fromA')
      expect(names).not.toContain('fromB')
    })
    test('imports are followed through several layers, exposing only direct names', async () => {
      mockFS({
        'a.scm': '(import "b.scm")\n(define fromA 1)',
        'b.scm': '(import "c.scm")\n(define fromB 2)',
        'c.scm': '(define fromC 3)',
      })
      const names = (await treeOf('(import "a.scm")'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(names).toContain('fromA')
      expect(names).not.toContain('fromB')
      expect(names).not.toContain('fromC')
    })
    test('a failed import degrades gracefully (no crash, no names)', async () => {
      mockFS({})
      const names = (await treeOf('(import "gone.scm")\n(define local 1)'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(names).toContain('local')
    })
    test('a qualified import does not surface its names in the flat scope', async () => {
      // Names of a module imported under an alias are reachable only as
      // `alias.member`, so they must not appear in the flat visible set.
      const qualified = (await treeOf('(import image img)'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(qualified).not.toContain('rgb')
      expect(qualified).not.toContain('rgb-red')
      // Sanity: the unqualified form still surfaces them.
      const flat = (await treeOf('(import image)'))
        .getVisibleIdentifiers()
        .map((i) => i.name)
      expect(flat).toContain('rgb')
    })
  })
})

///// Scope checking: opt-in pass + runtime shadowing /////////////////////////

// N.B., scenarios preserved from main's suite. The default compile+run path
// (runProgram) deliberately does NOT run the scope-check pass -- it's opt-in
// (compile's `scopeCheck` option / CLI `--check`). So the error-detecting
// scenarios below are exercised through `scopeErrors` (the same expand +
// scopeCheckProgram the opt-in pass runs), which verifies real behavior today;
// shadowing (no errors expected) is pinned through runProgram as a runtime
// characterization. Some of these overlap the atomic cases above but exercise
// several errors in a single program.

test('letrec: a forward reference is in scope (a runtime, not a scope, concern)', async () => {
  // Every binder is in scope throughout, so referencing a later one is not a
  // scope error (it is caught at runtime when the still-unfilled slot is read).
  expect(
    await scopeErrors(`
(let
  ([x2 y2]
   [y2 5])
  (+ x2 y2))
`),
  ).toEqual([])
})

test('an undefined variable is reported by the scope-check pass', async () => {
  expect(await scopeErrors('(+ x 1)')).toEqual(["Undefined variable 'x'"])
})

test('duplicate binders in a lambda and a struct', async () => {
  expect(
    await scopeErrors(`
(lambda (x x y) (+ x x))
(struct foo (z y z))
`),
  ).toEqual([
    "Global variable 'foo-z' is already defined",
    "Duplicate variable 'x' encountered in binding list",
  ])
})

test('a repeated pattern variable in a match branch', async () => {
  expect(
    await scopeErrors(`
(match (list 1 2 3)
  [null "fail"]
  [(cons x x) "fail"])
`),
  ).toEqual(["Duplicate binding variable 'x' encountered in pattern"])
})

test('shadowing is legal and produces the expected runtime values', async () => {
  expect(
    await runProgram(`
(define x 3)
(define y (+ x 2))
(define x -5)
(+ x y)

(define f
  (lambda (x)
    (* x 2)))

(f 3)

(let
  ([z 10]
   [w (+ z x)]
   [v 100])
  (+ w v))

x
`),
  ).toEqual(['0', '6', '105', '-5'])
})
