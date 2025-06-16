export interface Spec {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
}
export declare const any: Spec;
export declare const and: (...specs: Spec[]) => Spec;
export declare const or: (...specs: Spec[]) => Spec;
export declare const boolean: {
    predicate: (v: any) => v is boolean;
    errorMsg: (actual: any) => string;
};
export declare const number: {
    predicate: (v: any) => v is number;
    errorMsg: (actual: any) => string;
};
export declare const string: {
    predicate: (v: any) => v is string;
    errorMsg: (actual: any) => string;
};
export declare const numRange: (min: number, max: number) => {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const integer: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const nat: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const pos: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const nonneg: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const func: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const char: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const pair: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const list: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const nonemptyList: {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const listof: (spec: Spec) => {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const vector: {
    predicate: (v: any) => v is any[];
    errorMsg: (actual: any) => string;
};
export declare const struct: (kind: string) => {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const equal: (expected: any) => {
    predicate: (v: any) => boolean;
    errorMsg: (actual: any) => string;
};
export declare const html: {
    predicate: (v: any) => v is HTMLElement;
    errorMsg: (actual: any) => string;
};
export type Contract = {
    funcName: string;
    params: Spec[];
    varargs?: Spec;
};
export declare const contract: (funcName: string, params: Spec[], varargs?: Spec) => Contract;
export declare function checkContract(args: IArguments, contract: Contract): void;
//# sourceMappingURL=contract.d.ts.map