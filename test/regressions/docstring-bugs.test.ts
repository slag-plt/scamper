import { describe, expect, test } from 'vitest'
import { ScamperDiagnostic } from '../../src/scheme/diagnostic'
import { compile, tokenizeAndParse } from '../../src/scheme'
import { parseFunctionDocFromComments } from '../../src/scheme/docstring/docstring'
import { scopeCheckProgram } from '../../src/scheme/scope'
import { expandProgram } from '../../src/scheme/expansion'
import { anyRange } from '../scheme/util'

describe('docstring bugs', () => {
  // `;` comments are not docstrings (`;;;`); they must not be treated as one.
  //
  // N.B., the comment sits directly above the define. It used to be written
  // with a blank line between them, back when a comment run walked back over
  // one; a docstring now attaches only to the definition directly below it (see
  // test/regressions/docstring-attachment.test.ts), which is what stops a file
  // header from being swallowed by the first definition in the file.
  test('should not attempt to parse a block of non-doc comments', () => {
    const { program: prog, diagnostics } = tokenizeAndParse(`
; test
(define x 1)
`)
    expect(diagnostics).toEqual([])
    expect(prog?.length).toBe(1)
    const stmt = prog?.[0]
    expect(stmt?.tag).toBe('define')
    if (stmt?.tag !== 'define') return
    // the raw comment is still captured (parsing is deferred, not skipped
    // outright) -- but parsing it as a docstring correctly yields nothing,
    // since a plain `;` comment isn't the `;;;` doc-comment format.
    expect(stmt.docComments).toEqual([{ line: '; test', range: anyRange }])
    expect(parseFunctionDocFromComments(stmt.docComments ?? []).doc).toBeUndefined()
  })

  // Docstring parsing is deferred out of the main compile pass entirely, so
  // a malformed docstring is a documentation-quality issue -- it must not
  // prevent otherwise-valid code from compiling and running.
  test('a malformed docstring does not block compilation', async () => {
    const src = `
;;; this is not a valid signature line at all, no arrow
(define add1 (lambda (x) (+ x 1)))
(display (add1 5))
`
    const { prog, diagnostics } = await compile(src)
    expect(diagnostics).toEqual([])
    expect(prog).toBeDefined()

    // confirm the docstring really is malformed (otherwise this test proves
    // nothing) -- parsing it on demand yields a diagnostic.
    const { program: parsed } = tokenizeAndParse(src)
    const stmt = parsed?.find((s) => s.tag === 'define')
    expect(stmt?.tag).toBe('define')
    if (stmt?.tag !== 'define') return
    expect(
      parseFunctionDocFromComments(stmt.docComments ?? []).diagnostics.length,
    ).toBeGreaterThan(0)
  })

  // #479: contract insertion attaches a `;;;` block to whatever define follows
  // it, so a helper slipped in between a docstring and the function it
  // documents silently takes that function's contract -- and leaves the
  // documented function with none. Both names are right there to compare, so
  // the mismatch is reported instead of compiled.
  test('a docstring over a differently-named define is reported, not silently applied to it', async () => {
    const src = [
      ';;; (f x) -> number?',
      ';;;  x : number?',
      ';;; Returns x.',
      '(define helper (lambda (a b c) a))',
      '(define f (lambda (x) x))',
    ].join('\n')
    const { diagnostics } = await compile(src, { insertContracts: true })
    expect(diagnostics.map((d) => `${d.phase}: ${d.message}`)).toEqual([
      'Docstring: Docstring signature names "f", but the definition below it is "helper". A docstring is attached to the definition directly beneath it: move that definition above the docstring block, or correct the name in the signature.',
    ])
  })

  test('docstring errors (parse failures and signature mismatches) are tagged phase "Docstring", not "Parse"', async () => {
    const src = `
;;; (add1 wrongname) -> number?
;;;  wrongname : number?
;;; Adds one to a number.
(define add1 (lambda (x) (+ x 1)))
`
    const { program: parsed } = tokenizeAndParse(src)
    expect(parsed).toBeDefined()
    if (!parsed) return
    const diagnostics: ScamperDiagnostic[] = []
    await scopeCheckProgram(diagnostics, expandProgram(parsed))
    expect(diagnostics.length).toBeGreaterThan(0)
    expect(diagnostics.every((d) => d.phase === 'Docstring')).toBe(true)
  })
})
