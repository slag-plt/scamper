import FS from './fs.js'

function firstNLines(str: string, n: number): string {
  return str.split('\n').slice(0, n).join('\n')
}

class FileChooser {

  fs: FS
  container: HTMLElement

  constructor (container: HTMLElement) {
    this.fs = new FS()
    this.container = container
  }

  downloadFile (filename: string): void {
    const contents = this.fs.loadFile(filename)
    const hidden = document.createElement('a')
    hidden.href = 'data:attachment/text;charset=utf-8,' + encodeURIComponent(contents)
    hidden.target = '_blank'
    hidden.download = filename
    hidden.click()
  }

  makeFileDiv (filename: string): HTMLElement {
    const ret = document.createElement('div')
    ret.setAttribute('role', 'button')
    ret.setAttribute('aria-label', 'open file')
    ret.setAttribute('tabindex', '0')
    ret.classList.add('file')

    const header = document.createElement('div')
    header.setAttribute('id', `file-${filename}-header`)
    header.innerText = filename
    header.classList.add('header')
    ret.appendChild(header)

    const preview = document.createElement('div')
    preview.classList.add('preview')
    preview.innerText = firstNLines(this.fs.loadFile(filename), 5)
    preview.innerText = firstNLines(this.fs.loadFile(filename), 5)
    ret.appendChild(preview)

    // lastModified.innerText = `Last modified: ${new Date(file.lastModified).toLocaleString()}`

    const actionBar = document.createElement('div')
    actionBar.classList.add('actions')
    ret.appendChild(actionBar)

    const downloadButton = document.createElement('button')
    downloadButton.classList.add('fa-solid')
    downloadButton.classList.add('fa-download')
    downloadButton.setAttribute('aria-label', `download ${filename}`)
    downloadButton.addEventListener('click', (e) => {
      this.downloadFile(filename)
      e.stopPropagation()
    })
    actionBar.appendChild(downloadButton)

    const renameButton = document.createElement('button')
    renameButton.classList.add('fa-solid')
    renameButton.classList.add('fa-pencil')
    renameButton.setAttribute('aria-label', `rename ${filename}`)
    renameButton.addEventListener('click', (e) => {
      const newName = prompt(`Enter a new filename for ${filename}.\n\n(Make sure ${filename} is not open in a separate tab!)`, filename)
      if (newName !== null && newName !== filename) {
        if (this.fs.fileExists(newName)) {
          alert(`File ${newName} already exists!`)
        } else {
          this.fs.renameFile(filename, newName)
          this.populateChooser()
        }
      }
      e.stopPropagation()
    })
    actionBar.appendChild(renameButton)

    const deleteButton = document.createElement('button')
    deleteButton.classList.add('fa-solid')
    deleteButton.classList.add('fa-trash')
    deleteButton.setAttribute('aria-label', `delete ${filename}`)
    deleteButton.addEventListener('click', (e) => {
      const shouldDelete = confirm(`Are you sure you want to delete ${filename}?\n\n(Make sure ${filename} is not open in a separate tab!)`)
      if (shouldDelete) {
        this.fs.deleteFile(filename)
        this.populateChooser()
      }
      e.stopPropagation()
    })
    actionBar.appendChild(deleteButton)

    ret.addEventListener('click', () => {
      const params = new URLSearchParams({ filename })
      window.open(`ide.html?${params.toString()}`)
    })

    return ret
  }

  makeNewFileDiv(): HTMLElement {
    const ret = document.createElement('div')
    ret.setAttribute('role', 'button')
    ret.setAttribute('aria-label', 'open new file')
    ret.setAttribute('tabindex', '0')
    ret.classList.add('file')
    
    const header = document.createElement('div')
    header.innerText = 'Create a new program'
    ret.appendChild(header)

    ret.addEventListener('click', (_e) => {
      const filename = prompt('Enter a file name for your new program.')
      if (filename !== null) {
        if (this.fs.fileExists(filename)) {
          alert(`File ${filename} already exists!`)
        } else {
          this.fs.saveFile(filename, `; ${filename}`)
          this.populateChooser()
        }
      }
    })

    return ret
  }

  populateChooser (): void {
    // N.B., empty the container and repopulate from scratch
    this.container.innerHTML = ''
    const files = this.fs.getFileList() 
    for (const file of files) {
      this.container.appendChild(this.makeFileDiv(file))
    } 
    this.container.appendChild(this.makeNewFileDiv())
  }
}

const chooser = new FileChooser(document.getElementById('content')!)
document.getElementById('version')!.innerText = `(${APP_VERSION})`
chooser.populateChooser()