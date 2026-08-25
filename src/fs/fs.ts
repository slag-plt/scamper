/**
 * A block of file contents.
 *
 * `Uint8Array<ArrayBuffer>` rather than a bare `Uint8Array`: the DOM's `Blob`
 * and the OPFS writer only accept a view over a *non-shared* buffer, and an
 * unparameterised `Uint8Array` might be over a `SharedArrayBuffer`. Every
 * producer here -- `new Uint8Array(...)`, `TextEncoder` -- makes exactly this,
 * so naming it once keeps the qualifier out of every signature below.
 */
export type Bytes = Uint8Array<ArrayBuffer>

/** Metadata regarding a file found in the file system. */
export interface FileEntry {
  name: string
  preview: string | null
  isDirectory: boolean
}

/**
 * @returns true iff `name` belongs to a file an app keeps for itself rather
 *          than one of the user's own. By convention a dotted name marks
 *          internal state -- a file's saved history, a config left behind by an
 *          older build -- and the file drawer hides them.
 */
export function isHiddenName(name: string): boolean {
  return name.startsWith('.')
}

/**
 * @returns true iff `entry` is one of the user's own files, i.e., a regular
 *          file whose name isn't internal. The file drawer and the zip export
 *          share this notion of "the user's files" so they never disagree.
 */
export function isUserFile(entry: FileEntry): boolean {
  return !entry.isDirectory && !isHiddenName(entry.name)
}

/**
 * What kind of file a name denotes, which decides how it is read and how the
 * editor treats it (#385).
 *
 * + `scamper` -- a program: the editor gives it the language, the LSP, the
 *   formatter, and the Run button
 * + `text` -- editable text that is not a program: a plain editor, no
 *   diagnostics from a language it isn't written in
 * + `binary` -- not text at all: read as bytes, never loaded into the editor
 */
export type FileKind = 'scamper' | 'text' | 'binary'

/**
 * Extensions whose contents are not text.
 *
 * A table rather than a look at the contents, because the answer has to be the
 * same on both sides of the network and has to be available *before* reading:
 * a video is declined rather than loaded and then judged. Anything absent is
 * text, so a name we have not thought of opens in the editor -- the safe way
 * round, since the alternative is refusing to open a student's file.
 */
const BINARY_EXTENSIONS = new Set([
  // images -- .svg is deliberately absent, being XML a student may want to edit
  'png', 'jpg', 'jpeg', 'gif', 'webp', 'bmp', 'ico', 'avif', 'tif', 'tiff',
  // audio and video
  'mp3', 'wav', 'ogg', 'oga', 'flac', 'm4a', 'aac', 'opus',
  'mp4', 'webm', 'mov', 'avi', 'mkv', 'm4v',
  // archives
  'zip', 'gz', 'bz2', 'xz', 'tar', 'tgz', '7z', 'rar', 'jar',
  // fonts and documents
  'woff', 'woff2', 'ttf', 'otf', 'eot', 'pdf',
  // compiled and opaque
  'wasm', 'exe', 'dll', 'so', 'dylib', 'bin', 'class', 'pyc', 'db', 'sqlite',
])

/** Extensions the browser renders directly, so the IDE can show them (#385). */
const IMAGE_EXTENSIONS = new Set([
  'png', 'jpg', 'jpeg', 'gif', 'webp', 'bmp', 'ico', 'avif',
])

/**
 * @returns `name`'s lower-cased extension without its dot, or '' if it has
 *          none. A leading dot is the start of an internal name rather than an
 *          extension, so `.gitignore` has none while `.hello.scm.history` has
 *          `history`.
 */
function extensionOf(name: string): string {
  const dot = name.lastIndexOf('.')
  return dot <= 0 ? '' : name.slice(dot + 1).toLowerCase()
}

/** @returns the kind of file `name` denotes. See {@link FileKind}. */
export function fileKindOf(name: string): FileKind {
  const extension = extensionOf(name)
  if (extension === 'scm') return 'scamper'
  return BINARY_EXTENSIONS.has(extension) ? 'binary' : 'text'
}

/** @returns true iff `name`'s contents are bytes rather than text. */
export function isBinaryName(name: string): boolean {
  return fileKindOf(name) === 'binary'
}

/**
 * @returns true iff `name` is an image the browser can render, so the IDE can
 *          show it in place of the editor. Always a binary name.
 */
export function isImageName(name: string): boolean {
  return IMAGE_EXTENSIONS.has(extensionOf(name))
}

/**
 * Refuses a text operation on a binary file.
 *
 * Decoding a PNG as UTF-8 and writing the result back is how a file gets
 * destroyed, and before #385 that is exactly what opening one did. Throwing
 * here means the mistake cannot be made quietly: binary contents travel
 * through `loadBytes`/`saveBytes`, which are lossless.
 *
 * @throws Error if `filename` names a binary file
 */
export function refuseBinary(filename: string): void {
  if (isBinaryName(filename)) {
    throw new Error(
      `${filename} holds bytes rather than text: use loadBytes/saveBytes`,
    )
  }
}

/*
 * A instance of FS provides Scamper with access to the system's underlying file
 * system.
 */
export interface FS {

  /**
   * @returns a promise that resolves to the list of files found in the file
   *          system
   */
  getFileList(): Promise<FileEntry[]>

  /** @returns a promise that resolves to true iff the given file exists */
  fileExists(filename: string): Promise<boolean>

  /**
   * @returns a promise that resolves tothe contents of the given file, assumed
   *          to exist
   * @throws Error if the file is a binary one -- see {@link refuseBinary}
   */
  loadFile(filename: string): Promise<string>

  /**
   * Saves the specified file to the file system, creating it if it doesn't
   * already exist.
   * @param filename the name of the file to save
   * @param contents the contents to save to the file
   * @throws Error if the file is a binary one -- see {@link refuseBinary}
   */
  saveFile(filename: string, contents: string): Promise<void>

  /**
   * @returns a promise that resolves to the bytes of the given file, assumed to
   *          exist
   *
   * The way to read a file whose contents are not text (#385). Works for a text
   * file too -- text is bytes that happen to be UTF-8 -- so nothing below this
   * interface has to know which it is holding.
   */
  loadBytes(filename: string): Promise<Bytes>

  /**
   * Saves `bytes` to the given file, creating it if it doesn't already exist.
   * The counterpart to {@link loadBytes}.
   */
  saveBytes(filename: string, bytes: Bytes): Promise<void>

  /**
   * Deletes the specified file from the file system.
   * @param filename the name of the file to delete
   */
  deleteFile(filename: string): Promise<void>

  /**
   * Renames the specified file in the file system, potentially overwriting the
   * destination file if it already exists.
   * @param from the source file to rename
   * @param to the destination file to rename to
   */
  renameFile(from: string, to: string): Promise<void>
}