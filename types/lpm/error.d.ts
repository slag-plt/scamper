import { Range } from './range.js';
/** Phases of scamper execution, used for the purposes of error reporting. */
type Phase = 'Parser' | 'Runtime';
/** Errors that arise during Scamper compilation and execution. */
export declare class ScamperError extends Error {
    phase: Phase;
    modName?: string;
    range?: Range;
    source?: string;
    constructor(phase: Phase, msg: string, modName?: string, range?: Range, source?: string);
    toString(): string;
}
/** Internal compiler errors arise due to bugs in Scamper. */
export declare class ICE extends Error {
    funcName: string;
    constructor(funcName: string, msg: string);
    toString(): string;
}
export {};
//# sourceMappingURL=error.d.ts.map