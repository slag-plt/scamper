import { Doc } from './api/docs.js'
import showdown from 'showdown'

import * as Audio from './api/audio.js'
import * as Prelude from './api/prelude.js'
import * as Image from './api/image.js'
import * as Lab from './api/lab.js'
import * as Music from './api/music.js'
import * as Test from './api/test.js'
import * as Canvas from './api/canvas.js'
import * as Html from './api/html.js'
import * as Reactive from './api/reactive.js'

const converter = new showdown.Converter()

const libs: [string, object][] = [
  ['prelude', Prelude],
  ['image', Image],
  ['lab', Lab],
  ['music', Music],
  ['test', Test],
  ['audio', Audio],
  ['canvas', Canvas],
  ['html', Html],
  ['reactive', Reactive]
]

// https://stackoverflow.com/questions/5251520/how-do-i-escape-some-html-in-javascript
function escape(s: string): string {
  let lookup: any = {
    '&': "&amp;",
    '"': "&quot;",
    '\'': "&apos;",
    '<': "&lt;",
    '>': "&gt;"
  }
  return s.replace(/[&"'<>]/g, c => lookup[c])
}

function entryId (module: string, name: string): string {
  return `${module}-${name}`
}

function makeEntry (converter: showdown.Converter, module: string, name: string, doc: Doc): string {
  return `<div id="${entryId(module, name)}" class="entry">${converter.makeHtml(doc.docToMarkdown())}</div>`
}

function populateApi (mod: string, lib: object): void {
  const index = document.querySelector<HTMLDivElement>('#index')!
  index.innerHTML = `<strong>${mod}</strong>`

  const content = document.querySelector<HTMLDivElement>('#entries')!

  const indexEntries: string[] = []
  const docEntries: string[] = []

  for (const entry in lib) {
    const doc = (lib as any)[entry] as Doc
    indexEntries.push(`<li><a href="#${entryId(mod, entry)}">${escape(doc.name)}</a></li>`)
    docEntries.push(makeEntry(converter, mod, entry, doc))
  }
  index.innerHTML += indexEntries.join('')
  content.innerHTML = docEntries.join('')
}

function populateModules (): void {
  const div = document.querySelector<HTMLDivElement>('#modules')!
  const items: string[] = []
  for (const [name, _] of libs) {
    items.push(`<button id="module-${name}">${name}</button>`)
  }
  div.innerHTML = `<ul>${items.join(' ⋅ ')}</ul>`
  for (const [name, lib] of libs) {
    document.querySelector<HTMLButtonElement>(`#module-${name}`)!
      .addEventListener('click', () => {
        populateApi(name, lib)
      })
  }
}

document.getElementById('version')!.innerText = `(${APP_VERSION})`
populateModules()
populateApi('prelude', Prelude)
