import { ICE } from '../../lang.js'
import { FsRequest, FsResponse } from './message-types.js'

function storageAvailable(type: any) {
  let storage
  try {
    storage = window[type] as any
    const x = '__storage_test__'
    storage.setItem(x, x)
    storage.removeItem(x)
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
        e.name === 'QuotaExceededError' ||
        // Firefox
        e.name === 'NS_ERROR_DOM_QUOTA_REACHED') &&
      // acknowledge QuotaExceededError only if there's something already stored
      storage &&
      storage.length !== 0
    )
  }
}

class FakeLocalStorage {
  map: Map<string, string> = new Map()

  length(): number {
    return this.map.keys.length
  }
  clear(): void {
    this.map.clear()
  }

  getItem(key: string): string | null {
    if (this.map.has(key)) {
      return this.map.get(key)!
    } else {
      return null
    }
  }

  setItem(key: string, value: string): void {
    this.map.set(key, value)
  }

  key(_index: number): string | null {
    throw new ICE('FakeLocalStorage.key', 'Not implemented')
  }

  removeItem(key: string): void {
    this.map.delete(key)
  }
}

export interface FileEntry {
  name: string
  preview: string | null
  isDirectory: boolean
}

export class FS {
  prefixed(s: string): string {
    return `scamper-${s}`
  }
  fileTag(filename: string): string {
    return `${this.prefixed('file')}-${filename}`
  }
  fileListTag = `${this.prefixed('files')}`
  lastOpenedTag = `${this.prefixed('last-opened')}`
  initializedTag = `${this.prefixed('fs-initialized')}`
  migratedTag = `${this.prefixed('fs-migrated')}`

  worker: Worker

  constructor() {
    if (!storageAvailable('localStorage')) {
      globalThis.localStorage = new FakeLocalStorage() as any
    }

    this.worker = new Worker(new URL('./worker.ts', import.meta.url), {
      type: 'module',
    })
  }

  static async create(): Promise<FS> {
    const fs = new FS()
    await fs.init()
    return fs
  }

  async init(): Promise<void> {
    const localStorageFilesExist =
      JSON.parse(localStorage.getItem(this.fileListTag)!) !== null
    const migrated = localStorage.getItem(this.migratedTag) !== null
    if (localStorageFilesExist && !migrated) {
      await this.migrateLocalStorage()
    }

    // create a new, empty `untitled.scm` file on first load
    const initialized = localStorage.getItem(this.initializedTag) !== null
    if (!localStorageFilesExist && !initialized) {
      localStorage.setItem(this.initializedTag, 'true')
      this.setLastOpened(await this.makeUntitledFile())
    }
  }

  // migrate local storage files to opfs
  async migrateLocalStorage(): Promise<void> {
    const fileList = JSON.parse(
      localStorage.getItem(this.fileListTag)!,
    ) as string[]
    for (const filename of fileList) {
      const contents = localStorage.getItem(this.fileTag(filename))!
      await this.saveFile(filename, contents)
    }

    localStorage.setItem(this.migratedTag, 'true')
  }

  async messageWorker(data: FsRequest): Promise<FsResponse> {
    return new Promise((resolve, reject) => {
      const handler = (event: MessageEvent<FsResponse>) => {
        this.worker.removeEventListener('message', handler)
        resolve(event.data)
      }

      this.worker.addEventListener('message', handler)
      this.worker.addEventListener('error', reject)

      this.worker.postMessage(data)
    })
  }

  async getFileList(): Promise<FileEntry[]> {
    const directoryHandle = await navigator.storage.getDirectory()
    const fileEntries: FileEntry[] = []

    for await (const [_, handle] of (directoryHandle as any).entries()) {
      const isDirectory = handle.kind === 'directory'
      let preview: string | null = null

      if (!isDirectory) {
        try {
          preview = await this.getFilePreview(handle as FileSystemFileHandle)
        } catch {
          preview = null
        }
      }

      fileEntries.push({
        name: handle.name,
        preview,
        isDirectory,
      })
    }

    return fileEntries.sort((a, b) => {
      if (a.isDirectory && !b.isDirectory) return -1
      if (!a.isDirectory && b.isDirectory) return 1
      return a.name.localeCompare(b.name)
    })
  }

  async getFilePreview(fileHandle: FileSystemFileHandle): Promise<string> {
    try {
      const file = await fileHandle.getFile()
      const text = await file.text()
      return text.split('\n').slice(0, 5).join('\n')
    } catch (e) {
      throw new Error(`Failed to get file preview: ${e}`)
    }
  }

  getLastOpened(): string {
    return localStorage.getItem(this.lastOpenedTag)!
  }

  setLastOpened(filename: string): void {
    localStorage.setItem(this.lastOpenedTag, filename)
  }

  async fileExists(filename: string): Promise<boolean> {
    let list = await this.getFileList()
    return list.some((file) => file.name === filename)
  }

  async loadFile(filename: string, lock?: boolean): Promise<string> {
    let workerResult = await this.messageWorker({
      type: 'ReadFile',
      path: filename,
      lock,
    })
    if (workerResult.type === 'FileContent') {
      return workerResult.content
    } else if (workerResult.type === 'Error') {
      throw new Error(workerResult.message)
    } else {
      throw new Error('Failed to communicate with file system worker')
    }
  }

  async saveFile(
    filename: string,
    content: string,
    lock?: boolean,
  ): Promise<void> {
    const directoryHandle = await navigator.storage.getDirectory()
    await directoryHandle.getFileHandle(filename, { create: true })
    let workerResult = await this.messageWorker({
      type: 'WriteFile',
      path: filename,
      content,
      lock,
    })
    if (workerResult.type === 'Error') {
      throw new Error(workerResult.message)
    }
  }

  async deleteFile(filename: string): Promise<void> {
    const directoryHandle = await navigator.storage.getDirectory()
    directoryHandle.removeEntry(filename)
  }

  async renameFile(from: string, to: string) {
    let workerResult = await this.messageWorker({
      type: 'MoveFile',
      source: from,
      destination: to,
    })
    if (workerResult.type === 'Error') {
      throw new Error(workerResult.message)
    }
  }

  async makeUntitledFile(): Promise<string> {
    const base = 'untitled'
    const exists = await this.fileExists(`${base}.scm`)
    if (!exists) {
      await this.saveFile(`${base}.scm`, '')
      return `${base}.scm`
    } else {
      let i = 0
      while (await this.fileExists(`${base}-${i}.scm`)) {
        i++
      }
      let filename = `${base}-${i}.scm`
      await this.saveFile(filename, '')
      return filename
    }
  }
  
  loadFileTag(filename: string): string {
    return localStorage.getItem(this.fileTag(filename))!
  }
}

export default FS
