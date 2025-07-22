import { Value } from "./lang";
import { EditorView } from "@codemirror/view";
export declare class SyntaxNode {
    syntax: Value.Syntax;
    value: string;
    simplename: string;
    index: number | null;
    children: SyntaxNode[];
    parent: SyntaxNode | null;
    constructor(syntax: Value.Syntax, index?: number | null);
    listify(): void;
    toString(indent?: string): string;
}
export declare class AST {
    syntax: Value.Syntax[];
    nodes: SyntaxNode[];
    labelMap: Map<SyntaxNode, HTMLButtonElement>;
    constructor(syntax: Value.Syntax[]);
    renderNode(node: SyntaxNode, level: number, isLast: boolean, editor: EditorView, indexInParent?: number, totalSiblings?: number): HTMLElement;
    render(output: HTMLElement, editor: EditorView): void;
    buildTreeHTML(node: SyntaxNode): HTMLElement;
    renderTree(output: HTMLElement, nodes: SyntaxNode[]): void;
}
//# sourceMappingURL=ast.d.ts.map