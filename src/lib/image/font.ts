import { checkContract, contract } from '../../contract.js'
import * as C from '../../contract.js'
import { emptyLibrary, Library, registerValue, Value } from '../../lang.js'

export const lib: Library = emptyLibrary()

export interface Font extends Value.Struct {
  [Value.structKind]: 'font',
  face: string,
  system: string,
  isBold: boolean,
  isItalic: boolean
}

export const fontS: C.Spec = {
  predicate: (v: any) => Value.isStructKind(v, 'font'),
  errorMsg: (actual: any) => `expected a font, received ${Value.typeOf(actual)}`
}

export function fontToFontString (f: Font, size: number): string {
  const fontString = `"${f.face}"${f.system ? `, ${f.system}` : ''}`
  return `${f.isItalic ? 'italic ' : ''}${f.isBold ? 'bold ' : ''}${size}px ${fontString}`
}

function fontPrim (face: string, system: string, isBold: boolean, isItalic: boolean): Font {
  return {
    [Value.scamperTag]: 'struct', [Value.structKind]: 'font',
    face, system, isBold, isItalic
  }
}

export function font (name: string, system?: string,
    isBold?: boolean, isItalic?: boolean): Font {
  checkContract(arguments, contract('font', [C.string], C.any))
  return fontPrim(name, system || 'sans-serif', isBold || false, isItalic || false)
}
registerValue('font', font, lib)
