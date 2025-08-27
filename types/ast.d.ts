import * as S from './scheme/syntax.js';
import { EditorView } from "@codemirror/view";
declare class SyntaxNode {
    syntax: S.Syntax;
    value: string;
    name: string;
    parentname: string | null;
    simplename: string;
    index: number | null;
    children: SyntaxNode[];
    parent: SyntaxNode | null;
    constructor(syntax: S.Syntax, parent?: string | null, index?: number | null);
    toString(indent?: string): string;
}
export declare class AST {
    syntax: S.Syntax[];
    nodes: SyntaxNode[];
    labelMap: Map<SyntaxNode, HTMLButtonElement>;
    constructor(syntax: S.Syntax[]);
    renderNode(node: SyntaxNode, level: number, isLast: boolean, editor: EditorView, indexInParent?: number, totalSiblings?: number): HTMLElement;
    render(output: HTMLElement, editor: EditorView): void;
    buildTreeHTML(node: SyntaxNode): HTMLElement;
    renderTree(output: HTMLElement, nodes: SyntaxNode[]): void;
    describe(): string;
}
export {};
//# sourceMappingURL=ast.d.ts.map