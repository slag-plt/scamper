// These are coverage placeholders, not real tests. audio.scm binds directly
// to the Web Audio API (AudioContext, oscillators, etc.), which needs a
// browser-API mocking strategy that is a separate, larger effort. Until that
// lands, this file just tracks which functions still need real tests.
import { test } from 'vitest'

test.todo('sample?')
test.todo('sample-node')
test.todo('context?')
test.todo('audio-context')
test.todo('audio-node?')
test.todo('pipeline?')
test.todo('audio-pipeline')
test.todo('oscillator-node')
test.todo('audio-file-node')
test.todo('delay-node')
test.todo('play-sample')
