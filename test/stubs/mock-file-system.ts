import { FS } from '../../src/fs/fs'

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
    // N.B., rejects for a file that isn't here, like both real implementations
    // (NodeFileSystem's readFile throws ENOENT, OPFS's getFileHandle throws
    // NotFoundError). Returning '' instead would make a *missing* file
    // indistinguishable from an *empty* one, which silently voids any test
    // asserting that something wrote an empty file.
    const contents = this.files.get(filename)
    if (contents === undefined) {
      return Promise.reject(
        new Error(`MockFileSystem: file "${filename}" does not exist`),
      )
    }
    return Promise.resolve(contents)
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
    const contents = this.files.get(from)
    if (contents === undefined) {
      return Promise.reject(
        new Error(`MockFileSystem: file "${from}" does not exist`),
      )
    }
    this.files.set(to, contents)
    this.files.delete(from)
    return Promise.resolve()
  }
}
