import { Doc } from './docs.js'
import fs from 'fs'
import showdown from 'showdown'

import * as Audio from './audio.js'
import * as Prelude from './prelude.js'
import * as Image from './image.js'
import * as Lab from './lab.js'
import * as Music from './music.js'
import * as Test from './test.js'
import * as Canvas from './canvas.js'
import * as Html from './html.js'

const libs: [string, object][] = [
  ['prelude', Prelude],
  ['image', Image],
  ['lab', Lab],
  ['music', Music],
  ['test', Test],
  ['audio', Audio],
  ['canvas', Canvas],
  ['html', Html]
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

function makeModulePage (converter: showdown.Converter, modName: string, mod: object): string {
  let index = `<strong>${modName}</strong><ul>`
  let docs = ''
  for (const entry in mod) {
    const doc = (mod as any)[entry] as Doc
    console.log(`Making entry for ${doc.name}...`)
    index += `<li><a href="#${entryId(modName, entry)}">${escape(doc.name)}</a></li>`
    docs += makeEntry(converter, modName, entry, doc)
  }
  index += '</ul>'
  return `
  <html>
    <head>
      <style>
        #content {
          display: flex;
          flex-direction: row;
          width: 100%;
          height: 95vh;
        }

        #index {
          margin: 1em;
          padding: 1em;
          background-color: #eee;
          font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
          width: 40%;
          height: 100%;
          overflow-x: auto;
          overflow-y: scroll;
        }

        #index ul,li {
          list-style-type: none;
          list-style-position:inside;
          margin: 0;
          padding: 0;
        }

        #docs {
          height: 100%;
          overflow: scroll;
        }

        .entry {
          border: 1px solid black;
          padding: 1em;
          margin: 1em;
          font: 1em;
          font-family: font-family: -apple-system, BlinkMacSystemFont, avenir next, avenir, segoe ui, helvetica neue, helvetica, Cantarell, Ubuntu, roboto, noto, arial, sans-serif;
        }
        .entry code {
          font-family: Menlo, Consolas, Monaco, Liberation Mono, Lucida Console, monospace;
        }
      </style>
    </head>
    <body>
    <div id="content">
      <div id="index">${index}</div>
      <div id="docs">${docs}</div>
    </div>
  </head>
</html>
  `
}

const converter = new showdown.Converter()
for (const [modName, mod] of libs) {
  fs.open(`dist/docs/${modName}.html`, 'w', (err, fd) => {
    console.log(`Generating docs for ${modName}...`)
    if (err) { throw err }
    fs.write(fd, makeModulePage(converter, modName, mod), (err) => {
      if (err) { throw err }
      fs.close(fd, (err) => {
        if (err) { throw err }
      })
    })
  })
}

