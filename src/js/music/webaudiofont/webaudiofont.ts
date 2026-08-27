import * as L from '../../../lpm'
import { WebAudioFontPlayer } from './WebAudioFontPlayer.js'

function mkToneIndex(instrument: number): string {
  return `${instrument}`.padStart(3, '0') + '0'
}

// function mkPercIndex (instrument: number): string {
//   return `${instrument}`.padStart(2, '0')
// }

function mkTonePath(instrument: number, fontName: string): string {
  return `https://surikov.github.io/webaudiofontdata/sound/${mkToneIndex(instrument)}_${fontName}_sf2_file.js`
}

function mkToneId(instrument: number, fontName: string): string {
  return `_tone_${mkToneIndex(instrument)}_${fontName}_sf2_file`
}

function mkPercPath(instrument: number, fontName: string): string {
  return `https://surikov.github.io/webaudiofontdata/sound/128${instrument}_0_${fontName}_sf2_file.js`
}

function mkPercId(instrument: number, fontName: string): string {
  return `_drum_${instrument}_0_${fontName}_sf2_file`
}

/**
 * The slice of the vendored WebAudioFont player that we actually use.
 *
 * `WebAudioFontPlayer.ts` is carried as-is and opens with `@ts-nocheck`, so it
 * offers no types of its own. Rather than let that `any` spread through the
 * music library, this declares the four methods the library calls; anything
 * else it can do is deliberately not described here.
 */
interface WafLoader {
  startLoad(ctx: AudioContext, path: string, name: string): void
  waitLoad(onLoaded: () => void): void
}

interface WafPlayer {
  loader: WafLoader
  /** `preset` is an instrument object webaudiofont injected into the page. */
  queueWaveTable(
    ctx: AudioContext,
    destination: AudioNode,
    preset: unknown,
    when: number,
    pitch: number,
    duration: number,
    volume?: number,
  ): void
  cancelQueue(ctx: AudioContext): void
}

/**
 * The page itself, as webaudiofont uses it: instruments are injected into the
 * global scope under a generated name, and our per-browser singleton is kept
 * there too. `unknown` rather than `any` because nothing here should be called
 * or dereferenced without saying what it is first.
 */
type WafGlobals = Record<string, unknown> & { wafInstance?: Player }

const globals = (): WafGlobals => window as unknown as WafGlobals

class Player {
  fontName: string
  player: WafPlayer
  audioContext: AudioContext

  loadInstrument(instr: number, isPercussion = false): void {
    const path = isPercussion
      ? mkPercPath(instr, this.fontName)
      : mkTonePath(instr, this.fontName)
    const name = isPercussion
      ? mkPercId(instr, this.fontName)
      : mkToneId(instr, this.fontName)
    // console.log(`loading ${name} (${path})...`)
    // N.B., webaudiofont injects the instrument object into the global scope
    //       so we test for the presence of the injected name to determine
    //       if loading is necessary.
    // TODO: this is obviously prone to race conditions, in particular if a
    //       user mashes on the play button. Pre-loading mitigates this
    //       but ideally we would make this load synchronous somehow.
    if (globals()[name] === undefined) {
      this.player.loader.startLoad(this.audioContext, path, name)
      this.player.loader.waitLoad(() => {
        // TODO: is there any way to make the load synchronous so that
        //       preloading is not necessary?
        // console.log(`loaded ${name} (${path})!`)
      })
    }
  }

  constructor() {
    this.fontName = 'Chaos'
    // Through `unknown`: the vendored class is `@ts-nocheck`d, so TypeScript's
    // view of it is whatever it could infer, which does not line up with the
    // interface above. That interface is the contract, taken from what the
    // library documents and what we call.
    this.player = new WebAudioFontPlayer() as unknown as WafPlayer
    this.audioContext = new window.AudioContext()
  }

  /** @returns the injected instrument object, to be handed straight back to
   *  `queueWaveTable` -- we never look inside it. */
  getInstrument(id: number, isPercussion = false): unknown {
    this.loadInstrument(id, isPercussion)
    return globals()[
      isPercussion ? mkPercId(id, this.fontName) : mkToneId(id, this.fontName)
    ]
  }
}

/**
 * The player, or a runtime error saying why there is not one.
 *
 * `waf` answers undefined only outside a browser -- the CLI has no Web Audio --
 * and every music primitive needs it, so this is what they call. Before, each
 * asserted the answer away and a program run outside a browser died on "cannot
 * read properties of undefined" from inside the player.
 */
export function requireWaf(): Player {
  const player = waf()
  if (player === undefined) {
    throw new L.ScamperError(
      'Runtime',
      'The music library needs a browser: there is no audio here',
    )
  }
  return player
}

export function waf(): Player | undefined {
  // N.B., we want a _per-browser_ singleton, so we'll send this up to window!
  if (typeof window !== 'undefined') {
    const page = globals()
    page.wafInstance ??= new Player()
    return page.wafInstance
  } else {
    return undefined
  }
}
