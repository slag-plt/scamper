import { describe, expect, test } from 'vitest'
import { useRepl } from '../../../src/app/web/composables/use-repl'
import { initialize } from '../../../src/scamper'
import TextRenderer from '../../../src/lpm/renderers/text'
import type { Repl, ReplEntry } from '../../../src/app/web/composables/use-repl'

/** The REPL's most recent entry, or a failure saying there is none. */
function lastEntry(repl: Repl): ReplEntry {
  const last = repl.entries.value.at(-1)
  if (last === undefined) throw new Error('the REPL has no entries')
  return last
}


await initialize()

/** What an entry printed, as the text a person would have read. */
function shown(entry: ReplEntry): string {
  return entry.values.map((v) => TextRenderer.render(v)).join('\n')
}

// The transcript behind the REPL window (#399): entries, what they printed, and
// the banner saying what the session was seeded from.
describe('the REPL transcript', () => {
  test('opening seeds from the file and says so', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', '(define sq (lambda (n) (* n n)))')
    try {
      expect(repl.session.value).not.toBeNull()
      expect(repl.banner.value).toContain('lab.scm')
      // Seeding is silent: nothing in the transcript until something is run.
      expect(repl.entries.value).toEqual([])
      await repl.submit('(sq 5)')
      expect(shown(repl.entries.value[0])).toBe('25')
    } finally {
      repl.close()
    }
  })

  test('what the file prints is not in the transcript', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', '(display "hello")')
    try {
      expect(repl.entries.value).toEqual([])
    } finally {
      repl.close()
    }
  })

  test('an entry keeps what was typed beside what it printed', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', '')
    try {
      await repl.submit('(+ 1 2)')
      const [entry] = repl.entries.value
      expect(entry.source).toBe('(+ 1 2)')
      expect(shown(entry)).toBe('3')
      expect(entry.isRunning).toBe(false)
    } finally {
      repl.close()
    }
  })

  test('entries build on each other', async () => {
    const repl = useRepl()
    await repl.open(null, '')
    try {
      await repl.submit('(define x 41)')
      await repl.submit('(+ x 1)')
      expect(repl.entries.value.map(shown)).toEqual(['', '42'])
    } finally {
      repl.close()
    }
  })

  test('an error lands under the entry that caused it', async () => {
    const repl = useRepl()
    await repl.open(null, '')
    try {
      await repl.submit('(+ 1 2)')
      await repl.submit('(car 5)')
      expect(shown(repl.entries.value[0])).toBe('3')
      expect(shown(repl.entries.value[1])).toContain('expected pair')
    } finally {
      repl.close()
    }
  })

  test('a file that does not compile still opens a usable REPL', async () => {
    const repl = useRepl()
    await repl.open('broken.scm', '(define x')
    try {
      // The failure is reported where it happened, above the first entry, and
      // the banner says what is available instead.
      expect(repl.banner.value).toContain('did not run')
      expect(repl.entries.value.length).toBe(1)
      expect(shown(repl.entries.value[0])).not.toBe('')
      await repl.submit('(+ 1 2)')
      expect(shown(lastEntry(repl))).toBe('3')
    } finally {
      repl.close()
    }
  })

  test('a session with no file starts from the standard library', async () => {
    const repl = useRepl()
    await repl.open(null, '')
    try {
      expect(repl.banner.value).toContain('standard library')
      await repl.submit('(+ 1 2)')
      expect(shown(repl.entries.value[0])).toBe('3')
    } finally {
      repl.close()
    }
  })

  test('opening again throws the transcript away', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      await repl.submit('(define y 2)')
      expect(repl.entries.value.length).toBe(1)

      await repl.open('lab.scm', '(define x 1)')
      expect(repl.entries.value).toEqual([])
      // And the definitions made in the old session are gone with it.
      await repl.submit('y')
      expect(shown(repl.entries.value[0])).toContain('not found')
    } finally {
      repl.close()
    }
  })

  test('the history outlives the session it was typed in', async () => {
    // #458: Restart opens a new session, and the transcript goes with the old
    // one. What was typed is the person's, not the session's, so it stays --
    // including an entry that was refused, which is the one most worth
    // recalling to fix.
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      await repl.submit('(+ 1 2)')
      await repl.submit('(define a 1) (define b 2)')
      await repl.submit('(+ 1 2))')
      expect(repl.history.value).toEqual([
        '(+ 1 2)',
        '(define a 1) (define b 2)',
        '(+ 1 2))',
      ])

      await repl.open('lab.scm', '(define x 1)')
      expect(repl.entries.value).toEqual([])
      expect(repl.history.value).toEqual([
        '(+ 1 2)',
        '(define a 1) (define b 2)',
        '(+ 1 2))',
      ])
    } finally {
      repl.close()
    }
  })

  test('the history skips blank lines and immediate repeats', async () => {
    // As a shell's does: Enter on an empty prompt is not a command, and the
    // same one run twice in a row is worth one place in the history rather
    // than two. The same one run again later is not the one before it, so it
    // is kept.
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      await repl.submit('')
      await repl.submit('   ')
      await repl.submit('(+ 1 2)')
      await repl.submit('(+ 1 2)')
      await repl.submit('x')
      await repl.submit('(+ 1 2)')
      expect(repl.history.value).toEqual(['(+ 1 2)', 'x', '(+ 1 2)'])
    } finally {
      repl.close()
    }
  })

  test('an edit to the file marks the session stale', async () => {
    // Nothing is reconciled -- that is what makes it scratch work -- so saying
    // so is all the window can do.
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      expect(repl.isStale.value).toBe(false)
      repl.noteEdit()
      expect(repl.isStale.value).toBe(true)
      // What it already knows is unaffected: the session runs on regardless.
      await repl.submit('x')
      expect(shown(lastEntry(repl))).toBe('1')
    } finally {
      repl.close()
    }
  })

  test('restarting clears the warning', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      repl.noteEdit()
      await repl.open('lab.scm', '(define x 2)')
      expect(repl.isStale.value).toBe(false)
      await repl.submit('x')
      expect(shown(lastEntry(repl))).toBe('2')
    } finally {
      repl.close()
    }
  })

  test('an edit before a session is opened does not count', async () => {
    const repl = useRepl()
    repl.noteEdit()
    await repl.open('lab.scm', '(define x 1)')
    try {
      expect(repl.isStale.value).toBe(false)
    } finally {
      repl.close()
    }
  })

  test('the context is the file followed by the entries so far', async () => {
    // What the language server analyses the prompt inside, so a name from the
    // file or from an earlier entry is in scope while it is being typed.
    const repl = useRepl()
    await repl.open('lab.scm', '(define sq (lambda (n) (* n n)))')
    try {
      expect(repl.context.value).toBe('(define sq (lambda (n) (* n n)))')
      await repl.submit('(define x 41)')
      expect(repl.context.value).toBe(
        '(define sq (lambda (n) (* n n)))\n(define x 41)',
      )
    } finally {
      repl.close()
    }
  })

  test('an entry that was refused is not part of the context', async () => {
    // It never ran, so its definitions are not in the environment -- and were
    // it counted, the analysis would think they were.
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      await repl.submit('(define a 1) (define b 2)')
      expect(repl.context.value).toBe('(define x 1)')
    } finally {
      repl.close()
    }
  })

  test('an entry that does not compile is not part of the context', async () => {
    // Regression: one unparseable entry used to poison the context for the rest
    // of the session, silently costing completion every name the file and the
    // earlier entries had defined.
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      await repl.submit('(+ 1 2))')
      await repl.submit('(define y 2)')
      expect(repl.context.value).toBe('(define x 1)\n(define y 2)')
    } finally {
      repl.close()
    }
  })

  test('an entry that fails at run time still counts', async () => {
    // It parsed and it ran; what it did is the environment's business.
    const repl = useRepl()
    await repl.open(null, '')
    try {
      await repl.submit('(car 5)')
      expect(repl.context.value).toBe('(car 5)')
    } finally {
      repl.close()
    }
  })

  test('opening twice at once leaves the second session alone', async () => {
    // Regression: the first call's cleanup used to land on the second call's
    // session, clearing its busy flag mid-seed and replacing its entries.
    const repl = useRepl()
    const first = repl.open('one.scm', '(define x 1)')
    const second = repl.open('two.scm', '(define y 2)')
    await Promise.all([first, second])
    try {
      expect(repl.banner.value).toContain('two.scm')
      expect(repl.isBusy.value).toBe(false)
      await repl.submit('y')
      expect(shown(lastEntry(repl))).toBe('2')
      // And the session that was superseded took its definitions with it.
      await repl.submit('x')
      expect(shown(lastEntry(repl))).toContain('not found')
    } finally {
      repl.close()
    }
  })

  test('output with nothing typed yet is still shown', async () => {
    // What a handler the seeded file left running produces -- a timer, a key
    // handler -- reaches the session's channel without going through an entry.
    // Regression: it used to be dropped on the floor.
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    try {
      expect(repl.entries.value).toEqual([])
      await repl.session.value?.evaluate('(display "from a handler")')
      expect(repl.entries.value.length).toBe(1)
      expect(shown(repl.entries.value[0])).toContain('from a handler')
      // It is not an entry anyone typed, so it does not join the context.
      expect(repl.context.value).toBe('(define x 1)')
    } finally {
      repl.close()
    }
  })

  test('closing clears the context with everything else', async () => {
    const repl = useRepl()
    await repl.open('lab.scm', '(define x 1)')
    await repl.submit('(define y 2)')
    repl.close()
    expect(repl.context.value).toBe('')
  })

  test('closing ends the session', async () => {
    const repl = useRepl()
    await repl.open(null, '')
    await repl.submit('(+ 1 2)')
    repl.close()
    expect(repl.session.value).toBeNull()
    expect(repl.entries.value).toEqual([])
    // Submitting with nothing open is a no-op rather than a crash.
    await repl.submit('(+ 1 2)')
    expect(repl.entries.value).toEqual([])
  })

  test('a second entry cannot start while one is running', async () => {
    const repl = useRepl()
    await repl.open(null, '')
    try {
      await repl.submit('(define spin (lambda () (spin)))')
      const running = repl.submit('(spin)')
      await repl.submit('(+ 1 2)')
      // Refused rather than queued: the prompt is disabled while one runs.
      expect(repl.entries.value.length).toBe(2)
      repl.interrupt()
      await running
    } finally {
      repl.close()
    }
  })
})
