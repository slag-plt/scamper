import { Range, Env, Id, Value, Library, Code, Program } from './runtime.js';
/** Globals are a mapping from identifiers to values. */
export type Globals = Map<Id, Value>;
/**
 * An execution frame captures the ongoing execution of a function call:
 * + `ops`: the code currently being executed.
 * + `pc`: the program counter that tracks the current instructions.
 * + `values`: the value stack that holds intermediate results.
 * + `env`: the local environment.
 */
export type Frame = {
    code: Code;
    pc: number;
    values: Value[];
    env: Env;
};
/** Runtime options for the LPM */
export type Options = {
    maxArgs: number;
    maxCallStackDepth: number;
};
/**
 * An implementor of the Output interface is capable of receiving and
 * "rendering" LPM values that are displayed (via the `disp` op).
 */
export interface Output {
    send(v: Value): void;
}
/**
 * Tries to match a pattern against a value. A pattern is a value that
 * contains pattern variables (`PVar`s).
 * @param p the pattern
 * @param v the value
 * @param range the range of the pattern match in source code, if available
 * @returns an array of bindings, mapping local variable indices to bound
 *          values or `undefined` if pattern matching is not successful.
 */
export declare function tryMatch(p: Value, v: Value, range?: Range): [number, Value][] | undefined;
/**
 * The Little Pattern Machine (LPM), a stack-based virtual machine for executing
 * small functional programs.
 */
export declare class Machine {
    program: Program;
    globals: Globals;
    builtinLibs: Map<string, Library>;
    currentFrame?: Frame;
    dump: Frame[];
    output: Output;
    options: Options;
    constructor(program: Program, globals: Globals, builtinLibs: Map<string, Library>, entry: Id, output: Output, options: Options);
    /** Advance the program counter of the current frame forward by one instruction. */
    advancePc(): void;
    /**
     * Dumps the current frame onto the call stack and switches to a new
     * stack frame indicated by the given values.
     */
    dumpAndSwitch(values: Value[], env: Env, ops: Code, range?: Range): void;
    /** @returns if this machine has finished execution */
    isFinished(): boolean;
    /** Executes the machine from start to finish. */
    execute(): void;
    /** Executes a single step of the machine. */
    step(): void;
}
//# sourceMappingURL=machine.d.ts.map