import { OPFSFileSystem } from './fs.js'
import { Scamper } from '../scamper.js'
import { renderToOutput } from '../lpm/output/html.js'
import { initializeLibs } from '../lib/index.js'


async function createRunner(runTree: boolean): Promise<void> {
    const fs = await OPFSFileSystem.create()
    await initializeLibs()
    const params = new URLSearchParams(window.location.search)
    const outputPane = document.getElementById('output')
    if (outputPane === null) {
      throw new Error('Missing output element')
    }
    
    const filename = params.get('filename')
    if (filename === null) {
      renderToOutput(outputPane, new Error('No filename specified'))
      return
    }
    const currentFile = document.getElementById('current-file')
    if (currentFile === null) {
      throw new Error('Missing current-file element')
    }
    currentFile.innerText = filename    
    if (!(await fs.fileExists(filename))) {
      renderToOutput(outputPane, new Error(`File ${filename} does not exist`))
      return
    }

    const src = await fs.loadFile(filename)
    outputPane.innerHTML = ''
    try {
      if (runTree) {
        new Scamper(outputPane, src).runnerTree()
      } else {
        new Scamper(outputPane, src).runProgram()
      }
    } catch (e) {
      renderToOutput(outputPane, e)
    }
    const version = document.getElementById('version')
    if (version === null) {
      throw new Error('Missing version element')
    }
    version.innerText = `(${APP_VERSION})`  }


const params = new URLSearchParams(window.location.search);
const isTree = params.get("isTree") === "true";
await createRunner(isTree)