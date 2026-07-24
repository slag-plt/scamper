import * as L from '../../lpm'

export function html_isElement(v: any): boolean {
  return v instanceof HTMLElement
}

export function html_textAreaQ(v: any): boolean {
  return v instanceof HTMLTextAreaElement
}

export function html_buttonQ(v: any): boolean {
  return v instanceof HTMLButtonElement
}

export function html_textArea(id: string): HTMLTextAreaElement {
  const ret = document.createElement('textarea')
  ret.id = id
  return ret
}

export function html_textAreaGet(textArea: HTMLTextAreaElement): string {
  return textArea.textContent
}

export function html_button(label: string, fn: L.ScamperFn): HTMLButtonElement {
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

export function html_tag(name: string, ...children: L.Value[]): HTMLElement {
  const elt = document.createElement(name)
  if (children.length > 0 && L.isList(children[0])) {
    const attrs = L.listToVector(children[0])
    for (const attr of attrs) {
      if (L.isPair(attr)) {
        const pair = attr
        if (!L.isString(pair.fst)) {
          throw new L.ScamperError('Runtime', `attribute must be a string: ${L.toString(pair.fst)}`)
        } else if (!L.isString(pair.snd)) {
          throw new L.ScamperError('Runtime', `attribute value must be a string: ${L.toString(pair.snd)}`)
        } else {
          elt.setAttribute(pair.fst, pair.snd)
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

export function html_tagSetChildren(elt: HTMLElement, ...children: L.Value[]) {
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

export function html_onKeydown(fn: L.ScamperFn): void {
  window.addEventListener('keydown', (e) => {
    try {
      L.callScamperFn(fn, e.key)
    } catch (e) {
      alert(`on-keydown! callback threw an error:\n\n${(e as Error).toString()}`)
    }
  })
}
