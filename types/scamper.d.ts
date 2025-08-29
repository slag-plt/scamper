import * as LPM from './lpm';
import * as D from './display';
export declare class Scamper {
    display: D.HTMLDisplay;
    prog: LPM.Blk | undefined;
    machine: LPM.Machine | undefined;
    constructor(target: HTMLElement, src: string);
    runProgram(): void;
    runnerTree(): void;
    stepProgram(): void;
    stepStmtProgram(): void;
}
export default Scamper;
//# sourceMappingURL=scamper.d.ts.map