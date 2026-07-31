// These are coverage placeholders, not real tests. music.scm binds directly
// to the Web Audio API (via webaudiofont), which needs a browser-API mocking
// strategy that is a separate, larger effort. Until that lands, this file
// just tracks which functions still need real tests.
import { describe, test } from 'vitest'

describe('duration', () => {
  test.todo('dur?')
  test.todo('dur')
  test.todo('numerator')
  test.todo('denominator')
  test.todo('wn')
  test.todo('hn')
  test.todo('qn')
  test.todo('en')
  test.todo('sn')
  test.todo('tn')
})

describe('pitch and modifications', () => {
  test.todo('pitch?')
  test.todo('octave?')
  test.todo('mod?')
  test.todo('percussion')
  test.todo('tempo')
  test.todo('dynamics')
  test.todo('instrument')
  test.todo('note-handlers')
})

describe('composition', () => {
  test.todo('note-value?')
  test.todo('note')
  test.todo('note-freq')
  test.todo('repeat')
  test.todo('empty')
  test.todo('rest')
  test.todo('trigger')
  test.todo('par')
  test.todo('seq')
  test.todo('pickup')
  test.todo('mod')
  test.todo('note-event')
  test.todo('composition?')
})

describe('instruments and playback', () => {
  test.todo('load-instrument')
  test.todo('load-percussion')
  test.todo('use-high-quality-instruments')
  test.todo('make-note-handlers')
  test.todo('play-composition')
})
