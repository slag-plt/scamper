import { ScamperError, Value } from '../lang.js'
import * as L from '../lang.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import { callFunction } from '../sem.js'

const Html: L.Library = L.emptyLibrary()

function textArea (id: string): HTMLTextAreaElement {
  checkContract(arguments, contract('text-area', [C.string]))
  const ret = new HTMLTextAreaElement()
  ret.id = id
  return ret
}
L.registerValue('text-area', textArea, Html)

function textAreaGet (textArea: HTMLTextAreaElement): string {
  checkContract(arguments, contract('text-area-get', [C.any]))
  return textArea.textContent!
}
L.registerValue('text-area-get', textAreaGet, Html)

function button (label: string, fn: Function): HTMLButtonElement {
  checkContract(arguments, contract('button', [C.string, C.any]))
  const ret = document.createElement('button')
  ret.textContent = label
  ret.onclick = () => {
    try {
      callFunction(fn, [])
    } catch (e) {
      alert(`button callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
  }
  return ret
}
L.registerValue('button', button, Html)

function tag (name: string, ...children: Value.T[]): HTMLElement {
  checkContract(arguments, contract('tag', [C.string], C.any))
  const elt = document.createElement(name)
  if (children.length > 0 && L.Value.isList(children[0])) {
    const attrs = L.Value.listToVector(children[0] as L.Value.Pair)
    for (const attr of attrs) {
      if (L.Value.isPair(attr)) {
        const pair = attr as L.Value.Pair
        if (!L.Value.isString(pair.fst)) {
          throw new ScamperError('Runtime', `attribute must be a string: ${L.Value.toString(pair.fst)}`)
        } else if (!L.Value.isString(pair.snd)) {
          throw new ScamperError('Runtime', `attribute value must be a string: ${L.Value.toString(pair.snd)}`)
        } else {
          elt.setAttribute(pair.fst as string, pair.snd as string)
        }
      }
    }
    // N.B., drop the attribute list, leaving the children behind
    children = children.slice(1)
  }
  for (const child of children) {
    if (child instanceof HTMLElement) {
      elt.appendChild(child)
    } else {
      elt.textContent = child as string
    }
  }
  return elt
}
L.registerValue('tag', tag, Html)

function tagSetChildren (elt: HTMLElement, ...children: Value.T[]) {
  checkContract(arguments, contract('tag-set-children!', [C.any], C.any))
  if (!(elt instanceof HTMLElement)) {
    throw new ScamperError('Runtime', `tag-set-children! expects an HTML element, but received ${L.Value.typeOf(elt)}`)
  } else {
    children.forEach((e, i) => {
      if (!(e instanceof HTMLElement)) {
        throw new ScamperError('Runtime', `tag-set-children! expects all children to be HTML elements, but position ${i} is a ${L.Value.typeOf(elt)}$.`)
      }
    })
    // N.B., clear the current set of children
    elt.replaceChildren(children as unknown as Node)
    elt.textContent = ''
    for (const child of children) {
      elt.appendChild(child as HTMLElement)
    }
  }
}
L.registerValue('tag-set-children!', tagSetChildren, Html)

function onKeydown (fn: Function): void {
  checkContract(arguments, contract('on-keydown!', [C.func]))
  window.addEventListener('keydown', (e) => {
    try {
      callFunction(fn, e.key)
    } catch (e) {
      alert(`on-keydown! callback threw an error:\n\n${(e as Error).toString()}`)
    }
  })
}
L.registerValue('on-keydown!', onKeydown, Html)

export default Html