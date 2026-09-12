// @vitest-environment node
import { describe, expect, test } from 'vitest'
import { ScamperError } from '../../src/lpm'
import * as U from '../../src/lpm/util'
import {
  audio_audioPipeline,
  audio_sampleNode,
} from '../../src/js/audio/index'

// #535: #508 guarded six predicates and #514 guarded `toString`, but nine more
// `instanceof <DOM class>` tests were left bare. Each has the same shape:
// `instanceof` reads its right operand as an ordinary identifier, so where
// nothing declares the class the *test* is a `ReferenceError` rather than a
// `false`, and it takes down whatever was being decided.
//
// Most of the nine sit behind `requireBrowser` (#516) or a `context?` contract,
// so no program reaches them -- `(audio-pipeline 5 6)` reports "expected a
// context, received number" and stops there. The two cases below are the ones
// reachable from Javascript, which is what a test helper or a future
// non-browser entry point is. The rest are pinned by lint instead: see the
// `no-restricted-syntax` DOM-class entry in eslint.config.mjs, which is the
// durable half of this fix.
//
// N.B., this file overrides the suite's jsdom environment on purpose. Under
// jsdom `HTMLElement` and `AudioNode` *are* defined and none of this is
// visible. Node is the CLI's own condition.

/** The smallest stand-in for an AudioContext that `audio-pipeline` accepts. */
function stubContext(): AudioContext {
  return {
    sampleRate: 16000,
    createBuffer: () => ({ copyToChannel: () => undefined }),
    createBufferSource: () => ({ buffer: null }),
  } as unknown as AudioContext
}

describe('#535: bare `instanceof` against a DOM class where there is no DOM', () => {
  test('the DOM globals really are absent here', () => {
    expect(typeof HTMLElement).toBe('undefined')
    expect(typeof AudioNode).toBe('undefined')
  })

  // `audio-pipeline` resolves each argument to a node; anything that is neither
  // a node nor a sample is meant to be reported by name. Outside the browser
  // the `AudioNode` test threw first, so the report never happened.
  test('a bad pipeline source is reported rather than throwing', () => {
    expect(() => audio_audioPipeline(stubContext(), 6 as never)).toThrow(
      new ScamperError(
        'Runtime',
        'expected an audio node or a sample, received number',
      ),
    )
  })

  test('so is a bad pipeline effect, which is a separate test', () => {
    expect(() =>
      audio_audioPipeline(
        stubContext(),
        audio_sampleNode([0.5]),
        6 as never,
      ),
    ).toThrow(
      new ScamperError('Runtime', 'expected an audio node, received number'),
    )
  })

  // The Vue renderer picks a component for a value, which is a DOM-free
  // decision -- but its HTMLElement strategy sits ahead of the struct, error
  // and map ones, so *every* value that fell past the list strategies died
  // there instead of being rendered.
  test('the Vue renderer still picks a component for an ordinary value', async () => {
    const VueRenderer = (await import('../../src/lpm/renderers/vue')).default
    for (const v of [
      U.mkStruct('point', ['x', 'y'], [1, 2]),
      new ScamperError('Runtime', 'boom'),
      { a: 1 },
    ]) {
      expect(VueRenderer.render(v)).toBeDefined()
    }
  })
})
