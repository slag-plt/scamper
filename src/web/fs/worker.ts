import { FsRequest, FsResponse } from './message-types'

class FsWorker {
  private rootDir?: FileSystemDirectoryHandle
  private currFile: FileSystemSyncAccessHandle | null = null

  constructor() {
    self.onmessage = this.handleMessage.bind(this)
  }

  private async handleMessage(event: MessageEvent<FsRequest>) {
    try {
      if (!this.rootDir) {
        const navigator: WorkerNavigator = self.navigator
        this.rootDir = await navigator.storage.getDirectory()
      }

      const request = event.data

      if (!this.currFile && request.type !== 'MoveFile') {
        const path =
          request.type === 'ReadFile'
            ? request.path
            : request.type === 'WriteFile'
              ? request.path
              : ''
        this.currFile = await this.getFileHandle(path)
      }

      switch (request.type) {
        case 'ReadFile':
          await this.handleReadFile()
          break
        case 'WriteFile':
          await this.handleWriteFile(request.content)
          break
        case 'MoveFile':
          await this.handleMoveFile(request.source, request.destination)
          break
      }

      if (request.type !== 'MoveFile') {
        if (!request.lock) {
          this.currFile?.close()
          this.currFile = null
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

  private async handleReadFile() {
    if (!this.currFile) {
      throw new Error('No file handle available')
    }
    const content = await this.readFile(this.currFile)
    this.sendResponse({ type: 'FileContent', content })
  }

  private async handleWriteFile(content: string) {
    if (!this.currFile) {
      throw new Error('No file handle available')
    }
    await this.writeFile(this.currFile, content)
    this.sendResponse({ type: 'WriteComplete' })
  }

  private async handleMoveFile(oldPath: string, newPath: string) {
    // get old file handle
    const oldHandle = await this.rootDir!.getFileHandle(oldPath)

    // get sync handle to check if file is accessible
    try {
      const syncHandle = await (oldHandle as any).createSyncAccessHandle()
      syncHandle.close()
    } catch {
      this.sendResponse({ type: 'Error', message: 'File open in another tab' })
      return
    }

    // read old file contents
    const file = await oldHandle.getFile()
    const contents = await file.text()

    // create new file
    const newHandle = await this.rootDir!.getFileHandle(newPath, {
      create: true,
    })
    const newSyncHandle = await (newHandle as any).createSyncAccessHandle()
    await this.writeFile(newSyncHandle, contents)
    newSyncHandle.close()

    // remove old file
    await this.rootDir!.removeEntry(oldPath)

    this.sendResponse({ type: 'MoveComplete' })
  }

  private async getFileHandle(
    path: string,
  ): Promise<FileSystemSyncAccessHandle> {
    const fileHandle = await this.rootDir!.getFileHandle(path)
    try {
      return await (fileHandle as any).createSyncAccessHandle()
    } catch {
      throw new Error('File open in another tab')
    }
  }

  private async readFile(handle: FileSystemSyncAccessHandle): Promise<string> {
    const size = handle.getSize()
    const buffer = new Uint8Array(size)
    handle.read(buffer, { at: 0 })
    return new TextDecoder().decode(buffer)
  }

  private async writeFile(
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
    if (this.currFile) {
      this.currFile.close()
      this.currFile = null
    }
  }
}

const worker = new FsWorker()

self.addEventListener('unload', () => {
  worker.cleanup()
})
