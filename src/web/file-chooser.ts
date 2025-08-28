import { FS, FileEntry } from './fs/fs.js'

class FileChooser {
  fs?: FS
  container: HTMLElement

  constructor (container: HTMLElement) {
    this.container = container
  }

  static async create (container: HTMLElement): Promise<FileChooser> {
    const chooser = new FileChooser(container)
    await chooser.init()
    return chooser
  }

  async init (): Promise<void> {
    this.fs = await FS.create()
  }

  async downloadFile (filename: string): Promise<void> {
    if (!this.fs) {
      throw new Error('FileChooser: must call init() before usage')
    }

    const contents = await this.fs.loadFile(filename)
    const hidden = document.createElement('a')
    hidden.href = 'data:attachment/text;charset=utf-8,' + encodeURIComponent(contents)
    hidden.target = '_blank'
    hidden.download = filename
    hidden.click()
  }

  makeFileDiv (file: FileEntry): HTMLElement {
    const ret = document.createElement('div')
    ret.setAttribute('role', 'button')
    ret.setAttribute('aria-label', `Open ${file.name}`)
    ret.setAttribute('tabindex', '0')
    ret.classList.add('file')

    const header = document.createElement('div')
    header.setAttribute('id', `file-${file.name}-header`)
    header.innerText = file.name
    header.classList.add('header')
    ret.appendChild(header)

    const preview = document.createElement('div')
    preview.classList.add('preview')
    ret.appendChild(preview)

    const lastModified = document.createElement('div')
    lastModified.classList.add('last-modified')
    ret.appendChild(lastModified)

    header.innerText = file.name
    preview.innerText = file.preview ?? ""
    // lastModified.innerText = `Last modified: ${new Date(file.lastModified).toLocaleString()}`

    const actionBar = document.createElement('div')
    actionBar.classList.add('actions')
    ret.appendChild(actionBar)

    const downloadButton = document.createElement('button')
    downloadButton.classList.add('fa-solid')
    downloadButton.classList.add('fa-download')
    downloadButton.setAttribute('aria-label', `download ${file.name}`)
    downloadButton.addEventListener('click', async (e) => {
      e.stopPropagation()
      await this.downloadFile(file.name)
    })
    actionBar.appendChild(downloadButton)

    const renameButton = document.createElement('button')
    renameButton.classList.add('fa-solid')
    renameButton.classList.add('fa-pencil')
    renameButton.setAttribute('aria-label', `rename ${file.name}`)
    renameButton.addEventListener('click', async (e) => {
      if (!this.fs) return
      e.stopPropagation()
      const newName = prompt(`Enter a new filename for ${file.name}.\n\n(Make sure ${file.name} is not open in a separate tab!)`, file.name)
      if (newName !== null && newName !== file.name) {
        if (await this.fs.fileExists(newName)) {
          alert(`File ${newName} already exists!`)
        } else {
          try {
            await this.fs.renameFile(file.name, newName)
            this.populateChooser()
          } catch (e) {
            if (e instanceof Error) {
              this.displayError(e.message)
            }
          }
        }
      }
    })
    actionBar.appendChild(renameButton)

    const deleteButton = document.createElement('button')
    deleteButton.classList.add('fa-solid')
    deleteButton.classList.add('fa-trash')
    deleteButton.setAttribute('aria-label', `delete ${file.name}`)
    deleteButton.addEventListener('click', async (e) => {
      if (!this.fs) return
      e.stopPropagation()
      const shouldDelete = confirm(`Are you sure you want to delete ${file.name}?`)
      if (shouldDelete) {
        await this.fs.deleteFile(file.name)
        this.populateChooser()
      }
    })
    actionBar.appendChild(deleteButton)

    ret.addEventListener('click', () => {
      const params = new URLSearchParams({ filename: file.name })
      window.open(`ide.html?${params.toString()}`)
    })

    return ret
  }

  makeNewFileDiv(): HTMLElement {
    const ret = document.createElement('div')
    ret.setAttribute('role', 'button')
    ret.setAttribute('aria-label', 'Create a new program')
    ret.setAttribute('tabindex', '0')
    ret.classList.add('file')

    const header = document.createElement('div')
    header.innerText = 'Create a new program'
    ret.appendChild(header)

    ret.addEventListener('click', async (_e) => {
      if (!this.fs) return
      const filename = prompt('Enter a file name for your new program.')
      if (filename !== null) {
        if (await this.fs.fileExists(filename)) {
          alert(`File ${filename} already exists!`)
        } else {
          await this.fs.saveFile(filename, `; ${filename}`)
          this.populateChooser()
        }
      }
    })

    return ret
  }

  async populateChooser (): Promise<void> {
    if (!this.fs) {
      throw new Error('FileChooser: must call init() before usage')
    }

    // N.B., empty the container and repopulate from scratch
    this.container.innerHTML = ''
    const files = await this.fs.getFileList()
    for (const file of files) {
      if (!file.isDirectory) {
        this.container.appendChild(this.makeFileDiv(file))
      }
    }
    this.container.appendChild(this.makeNewFileDiv())
  }

  displayError (error: string) {
    document.getElementById("loading-content")!.innerText = error
    document.getElementById("loading")!.style.display = "block"
  }
}

function loadFile (file: File) {
  console.log(`Loading ${file.name}...`)
  const reader = new FileReader()
  reader.onload = () => {
    console.log(reader.result)
  }
  reader.readAsText(file)
}

// N.B., taken from MDN:
// https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/File_drag_and_drop
document.getElementById('content')!.addEventListener('drop', (ev) => {
  console.log('file(s) dropped')
  ev.preventDefault()
  if (ev.dataTransfer?.items) {
    [...ev.dataTransfer.items].forEach((item) => {
      if (item.kind === 'file') {
        loadFile(item.getAsFile()!)
      }
    })
  } else {
    [...(ev.dataTransfer?.files ?? [])].forEach((file) => {
      loadFile(file)
    })
  }
})


document.getElementById('content')!.addEventListener('dragover', (ev) => {
  console.log('file(s) in drop zone')
  ev.preventDefault()
})

document.getElementById('version')!.innerText = `(${APP_VERSION})`

const chooser = await FileChooser.create(document.getElementById('content')!)
chooser.populateChooser()