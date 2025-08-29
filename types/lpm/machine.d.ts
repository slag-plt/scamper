import * as L from './lang.js';
import { ScamperError } from './error.js';
import { OutputChannel, ErrorChannel } from './output.js';
/** The Little Pattern Machine */
export declare class Machine {
    builtinLibs: Map<string, L.Library>;
    maxCallStackDepth: number;
    stepMatch: boolean;
    out: OutputChannel;
    err: ErrorChannel;
    mainThread: L.Thread;
    constructor(builtinLibs: Map<string, L.Library>, env: L.Env, blk: L.Blk, out: OutputChannel, err: ErrorChannel, maxCallStackDepth?: number, stepMatch?: boolean);
    isFinished(): boolean;
    static tryMatch(v: L.Value, p: L.Pat): [string, L.Value][] | undefined;
    reportAndUnwind(thread: L.Thread, err: ScamperError): void;
    stepThread(thread: L.Thread): void;
    evaluateThread(thread: L.Thread): L.Value;
    step(): void;
    evaluate(): L.Value;
}
//# sourceMappingURL=machine.d.ts.map