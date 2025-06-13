import { Value } from "./lang";
declare class SyntaxNode {
    syntax: Value.Syntax;
    value: string;
    children: SyntaxNode[];
    constructor(syntax: Value.Syntax);
    toString(indent?: string): string;
}
export declare class AST {
    syntax: Value.Syntax[];
    nodes: SyntaxNode[];
    constructor(syntax: Value.Syntax[]);
    render(output: HTMLElement): void;
}
export {};
//# sourceMappingURL=ast.d.ts.map