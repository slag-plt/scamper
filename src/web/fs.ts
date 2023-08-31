import { ICE } from '../lang.js'

function storageAvailable(type: any) {
  let storage;
  try {
    storage = window[type]
    const x = '__storage_test__';
    (storage as any).setItem(x, x);
    (storage as any).removeItem(x)
    return true
  } catch (e) {
    return (
      e instanceof DOMException &&
      // everything except Firefox
      (e.code === 22 ||
        // Firefox
        e.code === 1014 ||
        // test name field too, because code might not be present
        // everything except Firefox
        e.name === "QuotaExceededError" ||
        // Firefox
        e.name === "NS_ERROR_DOM_QUOTA_REACHED") &&
      // acknowledge QuotaExceededError only if there's something already stored
      storage &&
      storage.length !== 0
    )
  }
}

class FakeLocalStorage {
  map: Map<string, string> = new Map()

  length (): number { return this.map.keys.length }
  clear (): void { this.map.clear() }

  getItem (key: string): string | null {
    if (this.map.has(key)) {
      return this.map.get(key)!
    } else {
      return null
    }
  }

  setItem (key: string, value: string): void {
    this.map.set(key, value)
  }

  key (index: number): string | null {
    throw new ICE('FakeLocalStorage.key', 'Not implemented')
  }

  removeItem (key: string): void {
    this.map.delete(key)
  }
}

class FS {
  prefixed (s: string): string { return `scamper-${s}` }
  fileTag (filename: string): string { return `${this.prefixed('file')}-${filename}` }
  fileListTag = `${this.prefixed('files')}`
  lastOpenedTag = `${this.prefixed('last-opened')}`

  constructor () {
    if (!storageAvailable('localStorage')) {
      globalThis.localStorage = new FakeLocalStorage() as any
    }
    if (JSON.parse(localStorage.getItem(this.fileListTag)!) === null) {
      // N.B., syncs the empty file list to initialize the slot
      localStorage.setItem(this.fileListTag, JSON.stringify([]))
      this.setLastOpened(this.makeUntitledFile())
    }
  }

  getFileList (): string[] {
    return JSON.parse(localStorage.getItem(this.fileListTag)!) as string[]
  }

  getLastOpened(): string {
    return localStorage.getItem(this.lastOpenedTag)!
  }

  setLastOpened (filename: string): void {
    localStorage.setItem(this.lastOpenedTag, filename)
  }

  fileExists (filename: string): boolean {
    return this.getFileList().includes(filename)
  }

  loadFile (filename: string): string {
    return localStorage.getItem(this.fileTag(filename))!
  }

  saveFile (filename: string, contents: string): void {
    const fileList = this.getFileList()
    if (!fileList.includes(filename)) {
      fileList.push(filename)
      localStorage.setItem(this.fileListTag, JSON.stringify(fileList))
    }
    localStorage.setItem(this.fileTag(filename), contents)
  }

  deleteFile (filename: string): void {
    const fileList = this.getFileList()
    fileList.splice(fileList.indexOf(filename), 1)
    localStorage.setItem(this.fileListTag, JSON.stringify(fileList))
    localStorage.removeItem(this.fileTag(filename))
  }

  renameFile (from: string, to: string) {
    const contents = this.loadFile(from)
    this.saveFile(to, contents)
    this.deleteFile(from)
  }

  makeUntitledFile (): string {
    const base = 'untitled'
    if (!this.fileExists(`${base}.scm`)) {
      this.saveFile(`${base}.scm`, '')
      return `${base}.scm`
    } else {
      let i = 0
      while (this.fileExists(`${base}-${i}.scm`)) { i++ }
      let filename = `${base}-${i}.scm`
      this.saveFile(filename, '')
      return filename
    }
  }
}

export default FS