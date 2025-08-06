import { FsRequest, FsResponse } from './message-types.js';
export interface FileEntry {
    name: string;
    preview: string | null;
    isDirectory: boolean;
}
export declare class FS {
    prefixed(s: string): string;
    fileTag(filename: string): string;
    fileListTag: string;
    lastOpenedTag: string;
    initializedTag: string;
    migratedTag: string;
    worker: Worker;
    constructor();
    static create(): Promise<FS>;
    init(): Promise<void>;
    migrateLocalStorage(): Promise<void>;
    messageWorker(data: FsRequest): Promise<FsResponse>;
    getFileList(): Promise<FileEntry[]>;
    getFilePreview(fileHandle: FileSystemFileHandle): Promise<string>;
    getLastOpened(): string;
    setLastOpened(filename: string): void;
    fileExists(filename: string): Promise<boolean>;
    loadFile(filename: string, lock?: boolean): Promise<string>;
    saveFile(filename: string, content: string, lock?: boolean): Promise<void>;
    deleteFile(filename: string): Promise<void>;
    renameFile(from: string, to: string): Promise<void>;
    makeUntitledFile(): Promise<string>;
    directReadFile(filename: string): Promise<string>;
}
export default FS;
//# sourceMappingURL=fs.d.ts.map