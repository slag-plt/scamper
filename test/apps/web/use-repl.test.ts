import { describe, expect, test } from 'vitest'
import { useRepl } from '../../../src/app/web/composables/use-repl'
import { initialize } from '../../../src/scamper'
import TextRenderer from '../../../src/lpm/renderers/text'
import type { ReplEntry } from '../../../src/app/web/composables/use-repl'

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
      expect(shown(repl.entries.value.at(-1)!)).toBe('3')
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
      expect(shown(repl.entries.value.at(-1)!)).toBe('1')
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
      expect(shown(repl.entries.value.at(-1)!)).toBe('2')
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
