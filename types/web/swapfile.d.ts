import FS from "./fs.js";
/**
 * The contents of a swap file are a JSON object containing relevant metadata
 * including frozen editor state and document history. Throughout, we do not
 * generate swap files for hidden files (i.e., files that start with a dot) to
 * avoid cluttering the file system with swap files of swap files, etc.
 */
export type SwapData = {
    editorState: string | null;
    priorVersions: [string, string][];
};
/** @return the swap file name of the given file. */
export declare function computeSwapFilename(filename: string): string;
/** Saves the given file's swap data to the file system. */
export declare function saveSwapFile(fs: FS, filename: string, swap: SwapData): Promise<void> | undefined;
/** Loads the given file's swap data from the file system, if it exists. */
export declare function loadSwapFile(fs: FS, filename: string): Promise<SwapData | undefined>;
/** Deletes the given file's swap file, if it exists. */
export declare function deleteSwapFile(fs: FS, filename: string): Promise<void>;
//# sourceMappingURL=swapfile.d.ts.map