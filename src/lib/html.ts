import * as L from '../lpm'
import { checkContract, contract } from './contract.js'
import * as C from './contract.js'

const Html: L.Library = new L.Library()

function textArea (id: string): HTMLTextAreaElement {
  checkContract(arguments, contract('text-area', [C.string]))
  const ret = new HTMLTextAreaElement()
  ret.id = id
  return ret
}
Html.registerValue('text-area', textArea)

function textAreaGet (textArea: HTMLTextAreaElement): string {
  checkContract(arguments, contract('text-area-get', [C.any]))
  return textArea.textContent!
}
Html.registerValue('text-area-get', textAreaGet)

function button (label: string, fn: Function): HTMLButtonElement {
  checkContract(arguments, contract('button', [C.string, C.any]))
  const ret = document.createElement('button')
  ret.textContent = label
  ret.onclick = () => {
    try {
      L.callScamperFn(fn, [])
    } catch (e) {
      alert(`button callback threw an error:\n\n${(e as Error).toString()}`)
      return
    }
  }
  return ret
}
Html.registerValue('button', button)

function tag (name: string, ...children: L.Value[]): HTMLElement {
  checkContract(arguments, contract('tag', [C.string], C.any))
  const elt = document.createElement(name)
  if (children.length > 0 && L.isList(children[0])) {
    const attrs = L.listToVector(children[0] as L.List)
    for (const attr of attrs) {
      if (L.isPair(attr)) {
        const pair = attr
        if (!L.isString(pair.fst)) {
          throw new L.ScamperError('Runtime', `attribute must be a string: ${L.toString(pair.fst)}`)
        } else if (!L.isString(pair.snd)) {
          throw new L.ScamperError('Runtime', `attribute value must be a string: ${L.toString(pair.snd)}`)
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
Html.registerValue('tag', tag)

function tagSetChildren (elt: HTMLElement, ...children: L.Value[]) {
  checkContract(arguments, contract('tag-set-children!', [C.any], C.any))
  if (!(elt instanceof HTMLElement)) {
    throw new L.ScamperError('Runtime', `tag-set-children! expects an HTML element, but received ${L.typeOf(elt)}`)
  } else {
    children.forEach((e, i) => {
      if (!(e instanceof HTMLElement)) {
        throw new L.ScamperError('Runtime', `tag-set-children! expects all children to be HTML elements, but position ${i} is a ${L.typeOf(elt)}$.`)
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
Html.registerValue('tag-set-children!', tagSetChildren)

function onKeydown (fn: Function): void {
  checkContract(arguments, contract('on-keydown!', [C.func]))
  window.addEventListener('keydown', (e) => {
    try {
      L.callScamperFn(fn, e.key)
    } catch (e) {
      alert(`on-keydown! callback threw an error:\n\n${(e as Error).toString()}`)
    }
  })
}
Html.registerValue('on-keydown!', onKeydown)

export default Html