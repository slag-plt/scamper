declare class FS {
    prefixed(s: string): string;
    fileTag(filename: string): string;
    fileListTag: string;
    lastOpenedTag: string;
    constructor();
    getFileList(): string[];
    getLastOpened(): string;
    setLastOpened(filename: string): void;
    fileExists(filename: string): boolean;
    loadFile(filename: string): string;
    saveFile(filename: string, contents: string): void;
    deleteFile(filename: string): void;
    renameFile(from: string, to: string): void;
    makeUntitledFile(): string;
}
export default FS;
//# sourceMappingURL=fs.d.ts.map