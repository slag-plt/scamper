/** An entry from the file system */
export interface FileEntry {
    name: string;
    preview: string | null;
    isDirectory: boolean;
}
/**
 * A wrapper around the OPFS API that simplifies access to files with a
 * higher-level API.
 */
export declare class FS {
    private root?;
    private constructor();
    /** @returns a new file system instance for accessing the OPFS */
    static create(): Promise<FS>;
    /** @return the list of files found at the root of the file system */
    getFileList(): Promise<FileEntry[]>;
    /** @return a preview (prefix) of the file denoted by the given handle */
    getFilePreview(fileHandle: FileSystemFileHandle): Promise<string>;
    /** @return true iff the given file exists */
    fileExists(filename: string): Promise<boolean>;
    /** @return the contents of the given file, assumed to exist */
    loadFile(filename: string): Promise<string>;
    /** Saves `contents` to the given file, creating it if it doesn't already exist */
    saveFile(filename: string, contents: string): Promise<void>;
    deleteFile(filename: string): Promise<void>;
    /** Renames the `from` file to the `to`. */
    renameFile(from: string, to: string): Promise<void>;
}
export default FS;
//# sourceMappingURL=fs.d.ts.map