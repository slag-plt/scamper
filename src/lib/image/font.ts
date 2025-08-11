import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import * as R from '../../lpm/runtime.js'

export const lib: R.Library = new R.Library()

export interface Font extends R.Struct {
  [R.structKind]: 'font',
  face: string,
  system: string,
  isBold: boolean,
  isItalic: boolean
}

export const fontS: C.Spec = {
  predicate: (v: any) => R.isStructKind(v, 'font'),
  errorMsg: (actual: any) => `expected a font, received ${R.typeOf(actual)}`
}

export function fontToFontString (f: Font, size: number): string {
  const fontString = `"${f.face}"${f.system ? `, ${f.system}` : ''}`
  return `${f.isItalic ? 'italic ' : ''}${f.isBold ? 'bold ' : ''}${size}px ${fontString}`
}

function fontPrim (face: string, system: string, isBold: boolean, isItalic: boolean): Font {
  return {
    [R.scamperTag]: 'struct', [R.structKind]: 'font',
    face, system, isBold, isItalic
  }
}

export function font (name: string, system?: string,
    isBold?: boolean, isItalic?: boolean): Font {
  checkContract(arguments, contract('font', [C.string], C.any))
  return fontPrim(name, system || 'sans-serif', isBold || false, isItalic || false)
}
lib.registerValue('font', font)
