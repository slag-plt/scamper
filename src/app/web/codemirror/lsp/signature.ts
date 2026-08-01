import type {
  ParameterInformation,
  SignatureHelp,
  SignatureInformation,
} from 'vscode-languageserver-protocol'
import { enclosingCallAt } from '../../../../scheme/token'
import { FunctionDoc } from '../../../../scheme/docstring/docstring'
import { predToString } from '../../../../scheme/docstring/render'
import { lookupFunctionDoc } from './docs'

/**
 * Builds signature help for the call enclosing [offset]: the callee's
 * signature with the active argument highlighted.
 * @returns the signature help, or null when the cursor isn't in a call to a
 *          documented function
 */
export function signatureHelpAt(src: string, offset: number): SignatureHelp | null {
  const call = enclosingCallAt(src, offset)
  if (call === undefined) {
    return null
  }
  const found = lookupFunctionDoc(src, call.name)
  if (found === undefined) {
    return null
  }
  const { label, parameters } = buildSignature(call.name, found.doc)
  const signature: SignatureInformation = {
    label,
    parameters,
    documentation: { kind: 'markdown', value: found.doc.description },
  }
  return {
    signatures: [signature],
    activeSignature: 0,
    activeParameter: call.activeParam,
  }
}

/**
 * Renders a signature label (`(name a b & rest) -> ret`, matching
 * functionDocSignature) together with each parameter's `[start, end]` offset
 * range into that label, so the client can highlight the active argument.
 */
function buildSignature(
  name: string,
  doc: FunctionDoc,
): { label: string; parameters: ParameterInformation[] } {
  const ret = predToString(doc.signature.predicate)
  const params = [...doc.params, ...(doc.restParam ? [doc.restParam] : [])]
  if (params.length === 0) {
    return { label: `(${name}) -> ${ret}`, parameters: [] }
  }
  let label = `(${name}`
  const parameters: ParameterInformation[] = []
  params.forEach((p, i) => {
    const isRest = doc.restParam !== undefined && i === params.length - 1
    label += isRest ? ' & ' : ' '
    const start = label.length
    label += p.name
    const desc = p.description === undefined ? '' : ` — ${p.description}`
    parameters.push({
      label: [start, label.length],
      documentation: `${predToString(p.predicate)}${desc}`,
    })
  })
  label += `) -> ${ret}`
  return { label, parameters }
}
