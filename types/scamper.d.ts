import { Env, ParserOutput, Prog } from './lang.js';
import * as Sem from './sem.js';
export type ScamperOptions = {
    isTracing: boolean;
    isPrintingCode: boolean;
    initialEnv?: Env;
    defaultDisplay: boolean;
};
export declare function mkOptions(): ScamperOptions;
export declare class Scamper {
    env: Env;
    display: HTMLElement;
    isTracing: boolean;
    parseroutput: ParserOutput;
    prog: Prog;
    sem: Sem.Sem;
    constructor(display: HTMLElement, src: string, opts: ScamperOptions);
    runProgram(): void;
    stepProgram(): void;
    stepStmtProgram(): void;
}
export default Scamper;
//# sourceMappingURL=scamper.d.ts.map