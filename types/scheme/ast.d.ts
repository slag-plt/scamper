import * as L from '../lpm';
export type PWild = {
    tag: 'pwild';
    range: L.Range;
};
export type PVar = {
    tag: 'pvar';
    name: string;
    range: L.Range;
};
export type PLit = {
    tag: 'plit';
    value: L.Value;
    range: L.Range;
};
export type PCtor = {
    tag: 'pctor';
    name: string;
    args: Pat[];
    range: L.Range;
};
export type Pat = PWild | PVar | PLit | PCtor;
export type Lit = {
    tag: 'lit';
    value: L.Value;
    range: L.Range;
};
export type Var = {
    tag: 'var';
    name: string;
    range: L.Range;
};
export type App = {
    tag: 'app';
    head: Exp;
    args: Exp[];
    range: L.Range;
};
export type Lam = {
    tag: 'lam';
    params: string[];
    body: Exp;
    range: L.Range;
};
export type Let = {
    tag: 'let';
    bindings: {
        name: string;
        value: Exp;
    }[];
    body: Exp;
    range: L.Range;
};
export type Begin = {
    tag: 'begin';
    exps: Exp[];
    range: L.Range;
};
export type If = {
    tag: 'if';
    guard: Exp;
    ifB: Exp;
    elseB: Exp;
    range: L.Range;
};
export type Match = {
    tag: 'match';
    scrutinee: Exp;
    branches: {
        pat: Pat;
        body: Exp;
    }[];
    range: L.Range;
};
export type Quote = {
    tag: 'quote';
    value: L.Value;
    range: L.Range;
};
export type LetS = {
    tag: 'let*';
    bindings: {
        name: string;
        value: Exp;
    }[];
    body: Exp;
    range: L.Range;
};
export type And = {
    tag: 'and';
    exps: Exp[];
    range: L.Range;
};
export type Or = {
    tag: 'or';
    exps: Exp[];
    range: L.Range;
};
export type Cond = {
    tag: 'cond';
    branches: {
        test: Exp;
        body: Exp;
    }[];
    range: L.Range;
};
export type Section = {
    tag: 'section';
    exps: Exp[];
    range: L.Range;
};
export type Exp = Lit | Var | App | Lam | Let | Begin | If | Match | Quote | LetS | And | Or | Cond | Section;
export type Import = {
    tag: 'import';
    module: string;
    range: L.Range;
};
export type Define = {
    tag: 'define';
    name: string;
    value: Exp;
    range: L.Range;
};
export type Disp = {
    tag: 'display';
    value: Exp;
    range: L.Range;
};
export type StmtExp = {
    tag: 'stmtexp';
    expr: Exp;
    range: L.Range;
};
export type Struct = {
    tag: 'struct';
    name: string;
    fields: string[];
    range: L.Range;
};
export type Stmt = Import | Define | Disp | StmtExp | Struct;
export type Prog = Stmt[];
export declare const mkPWild: (range?: L.Range) => PWild;
export declare const mkPVar: (name: string, range?: L.Range) => PVar;
export declare const mkPLit: (value: L.Value, range?: L.Range) => PLit;
export declare const mkPCtor: (name: string, args: Pat[], range?: L.Range) => PCtor;
export declare const mkLit: (value: L.Value, range?: L.Range) => Lit;
export declare const mkVar: (name: string, range?: L.Range) => Var;
export declare const mkApp: (head: Exp, args: Exp[], range?: L.Range) => App;
export declare const mkLam: (params: string[], body: Exp, range?: L.Range) => Lam;
export declare const mkLet: (bindings: {
    name: string;
    value: Exp;
}[], body: Exp, range?: L.Range) => Let;
export declare const mkBegin: (exps: Exp[], range?: L.Range) => Begin;
export declare const mkIf: (guard: Exp, ifB: Exp, elseB: Exp, range?: L.Range) => If;
export declare const mkMatch: (scrutinee: Exp, branches: {
    pat: Pat;
    body: Exp;
}[], range?: L.Range) => Match;
export declare const mkQuote: (value: L.Value, range?: L.Range) => Quote;
export declare const mkLetS: (bindings: {
    name: string;
    value: Exp;
}[], body: Exp, range?: L.Range) => LetS;
export declare const mkAnd: (exps: Exp[], range?: L.Range) => And;
export declare const mkOr: (exps: Exp[], range?: L.Range) => Or;
export declare const mkCond: (branches: {
    test: Exp;
    body: Exp;
}[], range?: L.Range) => Cond;
export declare const mkSection: (exps: Exp[], range?: L.Range) => Section;
export declare const mkImport: (module: string, range?: L.Range) => Import;
export declare const mkDefine: (name: string, value: Exp, range?: L.Range) => Define;
export declare const mkDisp: (value: Exp, range?: L.Range) => Disp;
export declare const mkStmtExp: (expr: Exp, range?: L.Range) => StmtExp;
export declare const mkStruct: (name: string, fields: string[], range?: L.Range) => Struct;
export declare function patToString(pat: Pat): string;
export declare function expToString(e: Exp): string;
export declare function stmtToString(s: Stmt): string;
export declare function progToString(p: Prog): string;
//# sourceMappingURL=ast.d.ts.map