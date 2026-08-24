// Runs under real headless Chromium (see test/vitest.browser.config.ts): Web
// Audio has no jsdom implementation, so AudioContext and AudioNode only exist
// here.
//
// Regression test for #181: `audio-pipeline` documents `pipeline : audio-node?`,
// but `sample-node` yields a `sample?` -- a struct wrapping a Float32Array,
// with no `connect` method -- so the issue's repro died on a bare
// `TypeError: pipeline.connect is not a function`. A sample has no context to
// bind to when it is made, so the pipeline converts it against its own.
import { describe, expect, test } from 'vitest'
import * as L from '../../src/lpm'
import {
  audio_audioContext,
  audio_audioPipeline,
  audio_oscillatorNode,
  audio_pipelineQ,
  audio_sampleNode,
  audio_sampleQ,
} from '../../src/js/audio/index.js'

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

describe('#181: a sample can drive an audio pipeline', () => {
  test('the reported program builds a pipeline instead of throwing', () => {
    // (audio-pipeline (audio-context 4000) (sample-node ...)), as filed.
    withCtx(4000, (ctx) => {
      const result = audio_audioPipeline(ctx, audio_sampleNode(ramp(8000)))
      expect(audio_pipelineQ(result)).toBe(true)
    })
  })

  test('the sample becomes a real source node in the pipeline', () => {
    withCtx(4000, (ctx) => {
      const result = audio_audioPipeline(ctx, audio_sampleNode(ramp(64)))
      // Converted, not merely stored: what the pipeline holds must be
      // something the Web Audio graph can connect.
      expect(result.pipeline).toBeInstanceOf(AudioBufferSourceNode)
      expect(result.ctx).toBe(ctx)
    })
  })

  test('the sample itself is untouched, so play-sample still takes one', () => {
    const sample = audio_sampleNode(ramp(64))
    withCtx(4000, (ctx) => {
      audio_audioPipeline(ctx, sample)
    })
    expect(audio_sampleQ(sample)).toBe(true)
  })

  test('a sample further down the chain is refused, in words', () => {
    // It would become a source node, and a source node has no inputs, so Web
    // Audio would fail with an IndexSizeError naming neither the sample nor
    // the pipeline.
    withCtx(4000, (ctx) => {
      const osc = audio_oscillatorNode(ctx, 'sine', 440)
      expect(() => {
        audio_audioPipeline(ctx, osc, audio_sampleNode(ramp(64)))
      }).toThrow(/first argument/)
    })
  })

  test('an ordinary node pipeline still works', () => {
    // The behaviour that must survive the change.
    withCtx(4000, (ctx) => {
      const osc = audio_oscillatorNode(ctx, 'sine', 440)
      const result = audio_audioPipeline(ctx, osc)
      expect(result.pipeline).toBe(osc)
    })
  })

  test('something that is neither reports a Scamper error, not a TypeError', () => {
    withCtx(4000, (ctx) => {
      expect(() => {
        audio_audioPipeline(ctx, 42 as unknown as AudioNode)
      }).toThrow(L.ScamperError)
    })
  })
})
