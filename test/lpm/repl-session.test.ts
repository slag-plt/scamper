import { beforeAll, describe, expect, test } from 'vitest'
import Scamper, { initialize } from '../../src/scamper'
import { ScamperError } from '../../src/lpm/error'
import TextRenderer from '../../src/lpm/renderers/text'
import type { ErrorChannel, OutputChannel, Value } from '../../src/lpm'

beforeAll(async () => {
  await initialize()
})

/**
 * A channel that renders whatever it is sent, so a test can read a session's
 * transcript as the text a person would have seen.
 */
class Transcript implements OutputChannel, ErrorChannel {
  readonly lines: string[] = []
  totalSends = 0

  send(v: Value) {
    this.lines.push(TextRenderer.render(v))
    this.totalSends++
  }

  report(e: ScamperError) {
    this.lines.push(e.toString())
  }

  pushLevel() {
    /* a transcript is flat; sections are the output pane's concern */
  }

  popLevel() {
    /* as above */
  }

  get text(): string {
    return this.lines.join('\n')
  }
}

/** A session and the transcript it writes to, ended when the test is over. */
function open() {
  const out = new Transcript()
  const session = Scamper.getInstance().startRepl({ out, err: out })
  return { session, out }
}

// A REPL is one program written an entry at a time: each entry runs in the
// environment the last one left (#399).
describe('a REPL session', () => {
  test('carries a definition forward to the next entry', async () => {
    const { session, out } = open()
    await session.evaluate('(define x 41)')
    await session.evaluate('(+ x 1)')
    expect(out.text).toBe('42')
    session.end()
  })

  test('carries an import forward', async () => {
    // An import rebinds the top level rather than defining into it, so it is
    // worth its own case.
    const { session, out } = open()
    await session.evaluate('(import image)')
    await session.evaluate('(procedure? rectangle)')
    expect(out.text).toBe('#t')
    session.end()
  })

  test('a later entry sees a definition redefined by an earlier one', async () => {
    const { session, out } = open()
    await session.evaluate('(define x 1)')
    await session.evaluate('(define x 2)')
    await session.evaluate('x')
    expect(out.text).toBe('2')
    session.end()
  })

  test('starts from the standard library', async () => {
    const { session, out } = open()
    await session.evaluate('(map (lambda (n) (* n 2)) (list 1 2 3))')
    expect(out.text).toContain('2 4 6')
    session.end()
  })

  test('an error in one entry leaves the session usable', async () => {
    const { session, out } = open()
    await session.evaluate('(define x 41)')
    await session.evaluate('(car 5)')
    expect(out.text).toContain('expected pair')
    await session.evaluate('(+ x 1)')
    expect(out.lines.at(-1)).toBe('42')
    session.end()
  })

  // An entry is one statement, so none is refused as surely as two: running an
  // empty program and printing nothing reads as the REPL ignoring what was
  // typed.
  test('refuses an entry holding no statement at all', async () => {
    const { session, out } = open()
    expect(await session.evaluate('; just a comment')).toBe(false)
    expect(out.text).toContain('there is none here to run')
    expect(await session.evaluate('')).toBe(false)
    session.end()
  })

  test('refuses an entry holding more than one statement', async () => {
    const { session, out } = open()
    expect(await session.evaluate('(define x 1)\n(define y 2)')).toBe(false)
    expect(out.text).toContain('one statement at a time')
    // Refused outright rather than half-run: neither definition took.
    await session.evaluate('x')
    expect(out.text).toContain('x')
    expect(out.text).not.toContain('\n1')
    session.end()
  })

  test('accepts a struct, which is one statement to write', async () => {
    // It expands into a define per field, so a session counting statements
    // after expansion would refuse it.
    const { session, out } = open()
    await session.evaluate('(struct point (x y))')
    await session.evaluate('(point-x (point 1 2))')
    expect(out.text).toBe('1')
    session.end()
  })

  test('accepts a define-export, likewise', async () => {
    const { session, out } = open()
    await session.evaluate('(define-export z 5)')
    await session.evaluate('(+ z 1)')
    expect(out.text).toBe('6')
    session.end()
  })

  test('reports a syntax error without ending the session', async () => {
    const { session, out } = open()
    // Reported as not having run, so a caller can tell what became part of the
    // program from what did not.
    expect(await session.evaluate('(+ 1')).toBe(false)
    expect(out.lines.length).toBeGreaterThan(0)
    await session.evaluate('(+ 1 2)')
    expect(out.lines.at(-1)).toBe('3')
    session.end()
  })
})

// An entry that will not finish on its own has to be abandonable: a REPL is
// where a student tries things, including the ones that spin forever.
describe('interrupting a REPL entry', () => {
  test('settles the entry and leaves the session usable', async () => {
    const { session, out } = open()
    await session.evaluate('(define spin (lambda () (spin)))')
    const pending = session.evaluate('(spin)')
    // Long enough for the scheduler to have started on it.
    await new Promise((resolve) => setTimeout(resolve, 20))
    session.interrupt()
    // The point of the test: a cancelled task never reaches onComplete, so
    // without the session settling it here this await would never return.
    await pending
    expect(out.text).toContain('cancelled')
    await session.evaluate('(+ 1 2)')
    expect(out.lines.at(-1)).toBe('3')
    session.end()
  })

  test('interrupting an idle session does nothing', async () => {
    const { session, out } = open()
    await session.evaluate('(define x 1)')
    session.interrupt()
    await session.evaluate('x')
    expect(out.lines).toEqual(['1'])
    session.end()
  })

  test('a session ended mid-compile abandons what it was about to run', async () => {
    // Regression: compiling is asynchronous, so `end` could be past before the
    // program reached the scheduler. It was then scheduled on a run that had
    // been torn down -- running work that was abandoned, and leaving the caller
    // waiting on a promise nothing would ever settle.
    const { session, out } = open()
    const seeding = session.seed('(display "should not run")')
    session.end()
    expect(await seeding).toBe(false)
    expect(out.lines).toEqual([])
  })

  test('an entry submitted to an ended session does nothing', async () => {
    const { session, out } = open()
    await session.evaluate('(define x 1)')
    session.end()
    expect(await session.evaluate('(display 1)')).toBe(false)
    expect(out.lines).toEqual([])
  })

  test('ending a session twice is safe', async () => {
    const { session } = open()
    await session.evaluate('(define x 1)')
    session.end()
    expect(() => {
      session.end()
    }).not.toThrow()
  })
})

// The file the REPL was opened on seeds it: its definitions, without its
// output, which the person has already seen.
describe('seeding a REPL session', () => {
  test('entries start from what the file defined', async () => {
    const { session, out } = open()
    expect(await session.seed('(define sq (lambda (n) (* n n)))')).toBe(true)
    await session.evaluate('(sq 5)')
    expect(out.text).toBe('25')
    session.end()
  })

  test('what the file prints is discarded', async () => {
    const { session, out } = open()
    await session.seed('(display "hello")\n(define x 1)')
    expect(out.lines).toEqual([])
    await session.evaluate('x')
    expect(out.text).toBe('1')
    session.end()
  })

  test('a file that does not compile leaves a usable session', async () => {
    const { session, out } = open()
    expect(await session.seed('(define x')).toBe(false)
    expect(out.lines.length).toBeGreaterThan(0)
    await session.evaluate('(+ 1 2)')
    expect(out.lines.at(-1)).toBe('3')
    session.end()
  })

  test('a file that fails part way through keeps what it defined first', async () => {
    const { session, out } = open()
    await session.seed('(define x 1)\n(car 5)\n(define y 2)')
    expect(out.text).toContain('expected pair')
    await session.evaluate('x')
    expect(out.lines.at(-1)).toBe('1')
    session.end()
  })
})
