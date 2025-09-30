import FS from './fs.js';
export declare const lockfileName = ".scamper.lock";
export declare function acquireLockFile(fs: FS): Promise<boolean>;
export declare function releaseLockFile(fs: FS): Promise<void>;
//# sourceMappingURL=lockfile.d.ts.map