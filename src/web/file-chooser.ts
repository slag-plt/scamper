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
    ret.classList.add('file')

    const header = document.createElement('div')
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
    downloadButton.addEventListener('click', async (e) => {
      e.stopPropagation()
      await this.downloadFile(file.name)
    })
    actionBar.appendChild(downloadButton)

    const renameButton = document.createElement('button')
    renameButton.classList.add('fa-solid')
    renameButton.classList.add('fa-pencil')
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
    deleteButton.addEventListener('click', async (e) => {
      if (!this.fs) return
      e.stopPropagation()
      const shouldDelete = confirm(`Are you sure you want to delete ${file.name}?\n\n(Make sure ${file.name} is not open in a separate tab!)`)
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

const chooser = await FileChooser.create(document.getElementById('content')!)
document.getElementById('version')!.innerText = `(${APP_VERSION})`
chooser.populateChooser()