// Runs under real headless Chromium (see test/vitest.browser.config.ts): Web
// Audio has no jsdom implementation, so AudioContext and AudioNode only exist
// here.
//
// Regression test for #181. `audio-pipeline` is documented as taking
// `audio-node?`, but `sample-node` yields a `sample?` -- a struct wrapping a
// Float32Array, with no `connect` method. The docstring-derived contract
// rejected the sample outright:
//
//   Runtime error: (error) expected an audio-node, received [Struct: sample]
//
// (The bare `TypeError: pipeline.connect is not a function` quoted in the issue
// predates contract insertion.) A sample has no context to bind to at the point
// it is made, so the pipeline converts it against its own.
//
// These run the Scheme program rather than calling the JS directly, because the
// contract is where it actually failed -- and because a broken docstring
// silently drops that contract along with the docs entry.
import { beforeAll, describe, expect, test } from 'vitest'
import * as L from '../../src/lpm'
import { docRegistry, initializeLibs } from '../../src/lib'
import { runProgram } from '../harness.js'
import {
  audio_audioContext,
  audio_audioPipeline,
  audio_oscillatorNode,
  audio_sampleNode,
  sampleSourceNode,
} from '../../src/js/audio/index.js'

beforeAll(async () => {
  await initializeLibs()
})

/** A short ramp in [-1, 1], the shape `sample-node` accepts. */
function ramp(n: number): number[] {
  return Array.from({ length: n }, (_, i) => (i / n) * 2 - 1)
}

/** Runs `fn` with a context, closing it afterwards either way. */
function withCtx<T>(sampleRate: number, fn: (ctx: AudioContext) => T): T {
  const ctx = audio_audioContext(sampleRate)
  try {
    return fn(ctx)
  } finally {
    void ctx.close()
  }
}

// The fix's first attempt documented the parameter as `audio-node? or sample?`,
// which is three top-level forms rather than one predicate expression. That is
// a *recoverable* parse error, so the whole entry silently gets no doc -- and
// with it goes the docstring-derived contract, along with the docs site, search
// and hover entries. The repro then "passed" because the check had vanished.
//
// Behaviour alone cannot catch that once the JS handles samples, so assert the
// docstring parsed. `(or/p audio-node? sample?)` is the form the library
// already uses elsewhere (see prelude.scm's `car`).
describe('#181: audio-pipeline keeps its docstring, and so its contract', () => {
  test('the entry parses, with both parameters', () => {
    const doc = docRegistry.get('audio')?.get('audio-pipeline')
    expect(doc).toBeDefined()
    expect(doc?.params.map((p) => p.name)).toEqual(
      expect.arrayContaining(['ctx', 'pipeline']),
    )
  })

  test('the contract is still enforced for something unusable', async () => {
    // If the docstring breaks, this is what stops being checked.
    const out = await runProgram(
      '(import audio)\n(audio-pipeline (audio-context 4000) 42)',
    )
    expect(out.join()).toContain('error')
  })
})

describe('#181: a sample can drive an audio pipeline', () => {
  test('the reported program builds a pipeline instead of erroring', async () => {
    const out = await runProgram(
      '(import audio)\n' +
        '(audio-pipeline (audio-context 4000)\n' +
        '  (sample-node (vector-map sin (vector-range 8000))))',
    )
    expect(out).toHaveLength(1)
    expect(out[0]).toContain('audio-pipeline')
    expect(out.join()).not.toContain('error')
  })

  test('an ordinary node pipeline still works', async () => {
    // The behaviour that must survive the change.
    const out = await runProgram(
      '(import audio)\n' +
        '(define ctx (audio-context 4000))\n' +
        '(audio-pipeline ctx (oscillator-node ctx "sine" 440))',
    )
    expect(out.join()).not.toContain('error')
  })

  test('a sample further down the chain is refused, in words', async () => {
    // It would become a source node, and a source node has no inputs, so Web
    // Audio would fail with an IndexSizeError naming neither the sample nor
    // the pipeline.
    const out = await runProgram(
      '(import audio)\n' +
        '(define ctx (audio-context 4000))\n' +
        '(audio-pipeline ctx (oscillator-node ctx "sine" 440)\n' +
        '  (sample-node (vector 0.5)))',
    )
    expect(out.join()).toContain('first argument')
  })

  test('an empty sample is refused rather than failing inside Web Audio', async () => {
    // createBuffer rejects a zero-length buffer as a bare DOMException.
    const out = await runProgram(
      '(import audio)\n' +
        '(audio-pipeline (audio-context 4000) (sample-node (vector)))',
    )
    expect(out.join()).toContain('at least one value')
  })

  test('the pipeline is audible once started', async () => {
    // The point of the issue: not merely that it stops erroring. Render the
    // same graph the renderer builds and check something actually came out.
    const ctx = new OfflineAudioContext(2, 4000, 4000)
    const source = sampleSourceNode(ctx, audio_sampleNode(ramp(4000)))
    source.connect(ctx.destination)
    source.start()
    const rendered = await ctx.startRendering()
    const peak = Math.max(...rendered.getChannelData(0).map(Math.abs))
    expect(peak).toBeGreaterThan(0.5)
  })

  test('the conversion keeps the data, in both channels, at the context rate', () => {
    const ctx = new OfflineAudioContext(2, 4000, 4000)
    const data = ramp(64)
    const source = sampleSourceNode(ctx, audio_sampleNode(data))
    const buffer = source.buffer
    expect(buffer).not.toBeNull()
    expect(buffer?.numberOfChannels).toBe(2)
    expect(buffer?.length).toBe(data.length)
    // One frame per element, so the context's rate sets the duration.
    expect(buffer?.duration).toBeCloseTo(data.length / ctx.sampleRate, 6)
    expect(Array.from(buffer?.getChannelData(0) ?? [])).toEqual(
      Array.from(buffer?.getChannelData(1) ?? []),
    )
    expect(buffer?.getChannelData(0)[10]).toBeCloseTo(data[10], 5)
  })

  test('each call yields a fresh source, since one can only start once', () => {
    withCtx(4000, (ctx) => {
      const sample = audio_sampleNode(ramp(64))
      expect(sampleSourceNode(ctx, sample)).not.toBe(
        sampleSourceNode(ctx, sample),
      )
    })
  })

  test('something that is neither reports a Scamper error, not a TypeError', () => {
    withCtx(4000, (ctx) => {
      expect(() => {
        audio_audioPipeline(ctx, 42 as unknown as AudioNode)
      }).toThrow(L.ScamperError)
      // And the node path is untouched.
      const osc = audio_oscillatorNode(ctx, 'sine', 440)
      expect(audio_audioPipeline(ctx, osc).pipeline).toBe(osc)
    })
  })
})
