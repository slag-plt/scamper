import { FsRequest, FsResponse } from './message-types'

class OpenSyncFileHandles {
  private handles: Map<string, FileSystemSyncAccessHandle>
  rootDir?: FileSystemDirectoryHandle 

  constructor() {
    this.handles = new Map()
  }

  isInitialized (): boolean { return this.rootDir !== undefined }

  async init () {
    if (!this.rootDir) {
      const navigator: WorkerNavigator = self.navigator
      this.rootDir = await navigator.storage.getDirectory()
    }
  }

  async get (path: string): Promise<FileSystemSyncAccessHandle> {
    if (this.handles.has(path)) {
      return this.handles.get(path)!
    } else {
      const fileHandle = await this.rootDir!.getFileHandle(path)
      try {
        const handle = await (fileHandle as any).createSyncAccessHandle()
        this.handles.set(path, handle)
        return handle
      } catch {
        throw new Error(`File ${path} open in another tab`)
      }
    }
  }

  close (path: string) {
    if (this.handles.has(path)) {
      this.handles.get(path)!.close()
      this.handles.delete(path)
    }
  }

  cleanup () {
    for (const [_, handle] of this.handles) {
      handle.close()
    }
    this.handles.clear()
  }
}

class FsWorker {
  private handles: OpenSyncFileHandles

  constructor() {
    self.onmessage = this.handleMessage.bind(this)
    this.handles = new OpenSyncFileHandles()
  }

  private async handleMessage(event: MessageEvent<FsRequest>) {
    try {
      if (this.handles.isInitialized() === false) {
        await this.handles.init()

        // N.B., for debugging purposes, report the current storage limits
        // to the console on each request
        const storageInfo = await navigator.storage.estimate()
        console.log(`Storage usage: ${storageInfo.usage} / ${storageInfo.quota}`)
      }

      const request = event.data
      switch (request.type) {
        case 'ReadFile':
          await this.handleReadFile(request.path)
          break
        case 'WriteFile':
          await this.handleWriteFile(request.path, request.content)
          break
        case 'MoveFile':
          await this.handleMoveFile(request.source, request.destination)
          break
        case 'CloseFile':
          this.handles.close(request.path)
          this.sendResponse({ type: 'CloseComplete' })
          break
      }

      if (request.type !== 'MoveFile' && request.type !== 'CloseFile') {
        if (!request.lock) {
          this.handles.close(request.path)
        }
      }
    } catch (error) {
      this.sendResponse({
        type: 'Error',
        message: error instanceof Error ? error.message : 'Unknown error',
      })
    }
  }

  private sendResponse(response: FsResponse) {
    self.postMessage(response)
  }

  private async handleReadFile(path: string) {
    const handle = await this.handles.get(path)
    const content = await this.readFromHandle(handle)
    this.sendResponse({ type: 'FileContent', content })
  }

  private async handleWriteFile(path: string, content: string) {
    const handle = await this.handles.get(path)
    await this.writeToHandle(handle, content)
    this.sendResponse({ type: 'WriteComplete' })
  }

  private async handleMoveFile(oldPath: string, newPath: string) {
    // read old file contents
    console.log('reading old file')
    const oldHandle = await this.handles.get(oldPath)!
    const contents = await this.readFromHandle(oldHandle)
    const rootDir = this.handles.rootDir!

    // create new file
    console.log('getting new file handle')
    const newHandle = await rootDir.getFileHandle(newPath, {
      create: true,
    })
    console.log('writing to new file')
    try {
      const newSyncHandle = await (newHandle as any).createSyncAccessHandle()
      await this.writeToHandle(newSyncHandle, contents)
      newSyncHandle.close()
    } catch {
      throw new Error(`File ${newPath} open in another tab`)
    }

    // remove old file
    console.log(`removing old file ${oldPath}`)
    this.handles.close(oldPath)
    rootDir.removeEntry(oldPath, { recursive: true })

    this.sendResponse({ type: 'MoveComplete' })
  }

  private async readFromHandle(handle: FileSystemSyncAccessHandle): Promise<string> {
    const size = handle.getSize()
    const buffer = new Uint8Array(size)
    handle.read(buffer, { at: 0 })
    return new TextDecoder().decode(buffer)
  }

  private async writeToHandle(
    handle: FileSystemSyncAccessHandle,
    content: string,
  ): Promise<void> {
    const encoder = new TextEncoder()
    const buffer = encoder.encode(content)
    handle.truncate(0)
    handle.write(buffer, { at: 0 })
    handle.flush()
  }

  public cleanup() {
    this.handles.cleanup()
  }
}

const worker = new FsWorker()

self.addEventListener('unload', () => {
  worker.cleanup()
})
