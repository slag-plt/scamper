import { Id, Library, Range } from './lang.js';
import { Env, Prog, Op, Value } from './lang.js';
declare class Control {
    idx: number;
    ops: Op.T[];
    constructor(ops: Op.T[]);
    isEmpty(): boolean;
    next(): Op.T;
    toString(): string;
    jumpTo(label: Op.Label): void;
}
declare class ExecutionState {
    stack: Value.T[];
    env: Env;
    control: Control;
    dump: [Value.T[], Env, Control][];
    constructor(env: Env, ops: Op.T[]);
    isFinished(): boolean;
    dumpAndSwitch(stack: Value.T[], env: Env, ops: Op.T[], range?: Range): void;
    isDumpEmpty(): boolean;
    popDump(): void;
    jumpPast(label: Op.Label): void;
}
export declare function opsToValue(ops: Op.T[]): Value.T;
export declare function stateToExp(state: ExecutionState): Value.T | undefined;
export declare function tryMatch(p: Value.T, v: Value.T, range?: Range): [string, Value.T][] | undefined;
export declare function callFunction(fn: Value.Closure | Function, ...args: any): any;
export declare class Sem {
    display: HTMLElement;
    env: Env;
    prog: Prog;
    src: string;
    curStmt: number;
    state?: ExecutionState;
    builtinLibs: Map<Id, Library>;
    traces?: HTMLElement[];
    defaultDisplay: boolean;
    isPrintingCode: boolean;
    constructor(display: HTMLElement, builtinLibs: Map<Id, Library>, isTracing: boolean, defaultDisplay: boolean, isPrintingCode: boolean, env: Env, prog: Prog, src: string);
    isFinished(): boolean;
    isTracing(): boolean;
    appendToCurrentTrace(v: HTMLElement | string): void;
    advance(): void;
    tryPrintCurrentCodeSegment(): void;
    stepDefine(name: string, body: Op.T[], range: Range): void;
    stepImport(modName: string, range: Range): void;
    stepStruct(id: string, fields: string[]): void;
    stepDisplay(body: Op.T[], _range?: Range): void;
    stepExp(body: Op.T[]): void;
    step(): void;
    stepToNextStmt(): void;
    execute(): void;
}
export {};
//# sourceMappingURL=sem.d.ts.map