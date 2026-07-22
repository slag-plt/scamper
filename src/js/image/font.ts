import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import * as L from '../../lpm'

export interface Font extends L.Struct {
  [L.structKind]: 'font',
  face: string,
  system: string,
  isBold: boolean,
  isItalic: boolean
}

export function image_fontQ(v: any): boolean {
  return L.isStructKind(v, 'font')
}

export const image_fontS: C.Spec = {
  predicate: image_fontQ,
  errorMsg: (actual: any) => `expected a font, received ${L.typeOf(actual)}`
}

export function image_fontToFontString (f: Font, size: number): string {
  const fontString = `"${f.face}"${f.system ? `, ${f.system}` : ''}`
  return `${f.isItalic ? 'italic ' : ''}${f.isBold ? 'bold ' : ''}${size}px ${fontString}`
}

function fontPrim (face: string, system: string, isBold: boolean, isItalic: boolean): Font {
  return {
    [L.scamperTag]: 'struct', [L.structKind]: 'font',
    face, system, isBold, isItalic
  }
}

export function image_font (name: string, system?: string,
    isBold?: boolean, isItalic?: boolean): Font {
  checkContract(arguments, contract('font', [C.string], C.any))
  return fontPrim(name, system || 'sans-serif', isBold || false, isItalic || false)
}
