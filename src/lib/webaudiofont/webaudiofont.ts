import { WebAudioFontPlayer } from './WebAudioFontPlayer.js'

function mkToneIndex (instrument: number): string {
  return `${instrument}`.padStart(3, '0') + '0'
}

// function mkPercIndex (instrument: number): string {
//   return `${instrument}`.padStart(2, '0')
// }

function mkTonePath (instrument: number, fontName: string): string {
  return `https://surikov.github.io/webaudiofontdata/sound/${mkToneIndex(instrument)}_${fontName}_sf2_file.js`
}

function mkToneId (instrument: number, fontName: string): string {
  return `_tone_${mkToneIndex(instrument)}_${fontName}_sf2_file`
}

function mkPercPath (instrument: number, fontName: string): string {
  return `https://surikov.github.io/webaudiofontdata/sound/128${instrument}_0_${fontName}_sf2_file.js`
}

function mkPercId (instrument: number, fontName: string): string {
  return `_drum_${instrument}_0_${fontName}_sf2_file`
}

class Player {
  fontName: string
  player: any
  audioContext: AudioContext

  loadInstrument(instr: number, isPercussion: boolean = false): void {
    const path = isPercussion ? mkPercPath(instr, this.fontName)
                              : mkTonePath(instr, this.fontName)
    const name = isPercussion ? mkPercId(instr, this.fontName)
                              : mkToneId(instr, this.fontName)
    console.log(`loading ${name} (${path})...`)
    // N.B., webaudiofont injects the instrument object into the global scope
    //       so we test for the presence of the injected name to determine
    //       if loading is necessary.
    // TODO: this is obviously prone to race conditions, in particular if a
    //       user mashes on the play button. Pre-loading mitigates this
    //       but ideally we would make this load synchronous somehow.
    if ((window as any)[name] === undefined) {
      this.player.loader.startLoad(this.audioContext, path, name);
      this.player.loader.waitLoad(() => {
        // TODO: is there any way to make the load synchronous so that
        //       preloading is not necessary?
        console.log(`loaded ${name} (${path})!`)
      })
    }
  }

  constructor () {
    this.fontName = 'Chaos'
    this.player = new WebAudioFontPlayer()
    this.audioContext = new window.AudioContext()
  }

  getInstrument (id: number, isPercussion: boolean = false): any {
    this.loadInstrument(id, isPercussion)
    return (window as any)[isPercussion ? mkPercId(id, this.fontName) : mkToneId(id, this.fontName)]
  }
}

export function waf(): Player | undefined {
  // N.B., we want a _per-browser_ singleton, so we'll send this up to window!
  if (typeof window !== 'undefined' && (window as any)['wafInstance'] === undefined) {
    (window as any)['wafInstance'] = new Player()
    return (window as any)['wafInstance']
  } else {
    return undefined
  }
}
