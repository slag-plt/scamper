import { isBinaryName, isHiddenName, refuseBinary, Bytes, FS } from '../../src/fs/fs'

export class MockFileSystem implements FS {
  // Bytes, like the real backends, so a test that writes an image and reads it
  // back exercises the same invariant production does (#385).
  private files = new Map<string, Bytes>()
  private directories = new Set<string>()

  static create(): Promise<MockFileSystem> {
    return Promise.resolve(new MockFileSystem())
  }

  /**
   * Adds a directory to the listing. Nothing in the IDE creates one, but both
   * the file drawer and the zip export have to leave them alone.
   */
  addDirectory(name: string): void {
    this.directories.add(name)
  }

  getFileList() {
    return Promise.resolve([
      ...[...this.files.entries()].map(([name, bytes]) => ({
        name,
        // A preview is text, so a binary file has none -- matching opfs.ts and
        // node.ts, which do not read one to build a preview they cannot show.
        preview:
          isHiddenName(name) || isBinaryName(name)
            ? null
            : new TextDecoder().decode(bytes),
        isDirectory: false,
      })),
      ...[...this.directories].map((name) => ({
        name,
        preview: null,
        isDirectory: true,
      })),
    ])
  }

  fileExists(filename: string) {
    return Promise.resolve(this.files.has(filename))
  }

  loadFile(filename: string) {
    try {
      refuseBinary(filename)
    } catch (e) {
      return Promise.reject(e as Error)
    }
    return this.loadBytes(filename).then((bytes) =>
      new TextDecoder().decode(bytes),
    )
  }

  saveFile(filename: string, contents: string) {
    try {
      refuseBinary(filename)
    } catch (e) {
      return Promise.reject(e as Error)
    }
    return this.saveBytes(filename, new TextEncoder().encode(contents))
  }

  loadBytes(filename: string) {
    // N.B., rejects for a file that isn't here, like both real implementations
    // (NodeFileSystem's readFile throws ENOENT, OPFS's getFileHandle throws
    // NotFoundError). Returning '' instead would make a *missing* file
    // indistinguishable from an *empty* one, which silently voids any test
    // asserting that something wrote an empty file.
    const bytes = this.files.get(filename)
    if (bytes === undefined) {
      return Promise.reject(
        new Error(`MockFileSystem: file "${filename}" does not exist`),
      )
    }
    return Promise.resolve(bytes)
  }

  saveBytes(filename: string, bytes: Bytes) {
    this.files.set(filename, bytes)
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
