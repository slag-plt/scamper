import OPFSFileSystem from "./fs.js";

/**
 * The contents of a swap file are a JSON object containing relevant metadata
 * including frozen editor state and document history. Throughout, we do not
 * generate swap files for hidden files (i.e., files that start with a dot) to
 * avoid cluttering the file system with swap files of swap files, etc.
 */
export type SwapData = {
  // The most recent frozen state of the editor (incl. undo stack)
  editorState: string | null
  // The document history, mappings from (JSONed) Date objects to their contents
  // N.B., could "contents" just be codemirror editor states... ?
  priorVersions: [string, string][]
}

/** @return the swap file name of the given file. */
export function computeSwapFilename (filename: string) { return `.${filename}.swp` }

/** Saves the given file's swap data to the file system. */
export function saveSwapFile (fs: OPFSFileSystem, filename: string, swap: SwapData) {
  // Debug log removed
  if (!filename.startsWith('.')) {
    const swapFilename = computeSwapFilename(filename)
    return fs.saveFile(swapFilename, JSON.stringify(swap))
  }
}

/** Loads the given file's swap data from the file system, if it exists. */
export async function loadSwapFile (fs: OPFSFileSystem, filename: string): Promise<SwapData | undefined> {
  if (!filename.startsWith('.')) {
    const swapFilename = computeSwapFilename(filename)
    if (await fs.fileExists(swapFilename)) {
      return JSON.parse(await fs.loadFile(swapFilename))
    } else {
      const state = { editorState: null, priorVersions: [] }
      await fs.saveFile(swapFilename, JSON.stringify(state))
      return state
    }
  } else {
    return undefined
  }
}

/** Deletes the given file's swap file, if it exists. */
export async function deleteSwapFile (fs: OPFSFileSystem, filename: string) {
  if (!filename.startsWith('.')) {
    const swapFilename = computeSwapFilename(filename)
    if (await fs.fileExists(swapFilename)) {
      await fs.deleteFile(swapFilename)
    }
  }
}