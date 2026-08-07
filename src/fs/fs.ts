/** Metadata regarding a file found in the file system. */
export interface FileEntry {
  name: string
  preview: string | null
  isDirectory: boolean
}

/**
 * @returns true iff `name` belongs to a file an app keeps for itself rather
 *          than one of the user's own. By convention a dotted name marks
 *          internal state -- the IDE's config and lock files, a file's saved
 *          history -- and the file drawer hides them.
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
   */
  loadFile(filename: string): Promise<string>

  /** 
   * Saves the specified file to the file system, creating it if it doesn't
   * already exist.
   * @param filename the name of the file to save
   * @param contents the contents to save to the file
   */
  saveFile(filename: string, contents: string): Promise<void>

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