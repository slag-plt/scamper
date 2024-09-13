import { noRange, Range } from './range.js'

export type Phase = 'Parser' | 'Runtime'

export class ScamperError extends Error {
  phase: Phase
  modName?: string
  range?: Range
  source?: string

  constructor (phase: Phase, msg: string, modName?: string, range?: Range, source?: string) {
    super(msg)
    this.phase = phase
    this.modName = modName
    this.range = range
    this.source = source
  }

  toString(): string {
    const detail = `${this.modName ? this.modName : ''}${(this.range && this.range !== noRange) ? this.range.toString() : ''}`
    const src = this.source ? `(${this.source}) ` : ''
    return `${this.phase} error${detail.length > 0 ? ' [' + detail + ']' : ''}: ${src}${this.message}`
  }
}

export class ICE extends Error {
  funcName: string

  constructor (funcName: string, msg: string) {
    super(msg)
    this.funcName = funcName
  }

  toString(): string {
    return `ICE (${this.funcName}): ${this.message}\n${this.stack}`
  }
}