import Doc from './docs.js'
import fs from 'fs'
import showdown from 'showdown'

import * as Prelude from './prelude.js'
import * as Image from './image.js'
import * as Music from './music.js'

const libs: [string, object][] = [
  ['prelude', Prelude],
  ['image', Image],
  ['music', Music]
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

function jsToSchemeName (s: string): string {
  let ret = ''
  for (let i = 0; i < s.length; i++) {
    if (s[i] === s[i].toUpperCase()) {
      ret += '-' + s[i].toLowerCase()
    } else {
      ret += s[i]
    }
  }
  return ret
}

function entryId (module: string, name: string): string {
  return `${module}-${name}`
}

function makeEntry (converter: showdown.Converter, module: string, name: string, doc: Doc): string {
  return `<div id="${entryId(module, name)}" class="entry">${converter.makeHtml(doc.docToMarkdown())}</div>`
}

function makeModulePage (converter: showdown.Converter, modName: string, mod: object): string {
  let index = `${modName} <ul>`
  let docs = ''
  for (const name in mod) {
    index += `<li><a href="#${entryId(modName, name)}">${escape(name)}</a></li>`
    docs += makeEntry(converter, modName, name, (mod as any)[name] as Doc)
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
          width: 20%;
          height: 100%;
          overflow: scroll;
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

