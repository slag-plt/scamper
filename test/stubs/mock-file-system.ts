import { FS } from "../../src/fs/fs"

export class MockFileSystem implements FS {
  private files = new Map<string, string>()

  static create(): Promise<MockFileSystem> {
    return Promise.resolve(new MockFileSystem())
  }

  getFileList() {
    return Promise.resolve(
      [...this.files.entries()].map(([name, preview]) => ({
        name,
        preview,
        isDirectory: false,
      })),
    )
  }

  fileExists(filename: string) {
    return Promise.resolve(this.files.has(filename))
  }

  loadFile(filename: string) {
    return Promise.resolve(this.files.get(filename) ?? "")
  }

  saveFile(filename: string, contents: string) {
    this.files.set(filename, contents)
    return Promise.resolve()
  }

  deleteFile(filename: string) {
    this.files.delete(filename)
    return Promise.resolve()
  }

  renameFile(from: string, to: string) {
    const contents = this.files.get(from) ?? ""
    this.files.set(to, contents)
    this.files.delete(from)
    return Promise.resolve()
  }
}
