import FS from './fs/fs.js'
import { Scamper, mkOptions } from '../scamper.js'
import { renderToOutput } from '../display.js'

class Runner {
  constructor () {}

  static async create (runTree: boolean): Promise<void> {
    const fs = new FS()
    const params = new URLSearchParams(window.location.search)
    const outputPane = document.getElementById('output')!

    if (!params.has('filename')) {
      renderToOutput(outputPane, new Error('No filename specified'))
      return
    }
    
    const filename = params.get('filename')!
    document.getElementById('current-file')!.innerText = filename
    
    if (!fs.fileExists(filename)) {
      renderToOutput(outputPane, new Error(`File ${filename} does not exist`))
      return
    }

    const src = await fs.loadFileTag(filename)
    outputPane!.innerHTML = ''
    try {
      if (runTree) {
        new Scamper(outputPane, src, mkOptions()).runnerTree()
      } else {
        new Scamper(outputPane, src, mkOptions()).runProgram()
      }
    } catch (e) {
      renderToOutput(outputPane, e)
    }
    document.getElementById('version')!.innerText = `(${APP_VERSION})`
  }
}


const params = new URLSearchParams(window.location.search);
const isTree = params.get("isTree") === "true";
await Runner.create(isTree)