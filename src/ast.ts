import {Value} from "./lang";
import T = Value.T;
import isPair = Value.isPair;
import Pair = Value.Pair;
import mkSyntax = Value.mkSyntax;
import isArray = Value.isArray;
import Syntax = Value.Syntax;
import isSym = Value.isSym;
import Sym = Value.Sym;
import Vector = Value.Vector;
import isChar = Value.isChar;
import Char = Value.Char;
import {EditorView} from "@codemirror/view";
import {EditorSelection} from "@codemirror/state";

class SyntaxNode {
    syntax: Value.Syntax
    value: string = ''
    name: string = ''
    parentname: string | null = null;
    simplename: string = '';
    index: number | null = null;
    children: SyntaxNode[] = [];
    parent: SyntaxNode | null = null;

    constructor(syntax: Value.Syntax, parent: string | null = null, index: number | null = null) {
        this.syntax = syntax; 
        this.parentname = parent;
        this.index = index;
        let v: T = this.syntax.value;

        switch (typeof v) {
            case 'boolean':
                this.value = "Boolean "+v;
                this.name = "a boolean "+v;
                this.simplename = ''+v;
                break;
            case 'number':
                this.value = "Numerical "+v;
                this.name = "the number "+v;
                this.simplename = ''+v;
                break;
            case 'string':
                this.value = '"'+v+'"';
                this.name = 'the string "'+v+'"';
                this.simplename = '"'+v+'"';
                break;
            case 'undefined':
                this.value = "Undefined";
                this.name = 'an undefined value';
                this.simplename = 'undefined';
                break;
            case 'function':
                this.value = 'Function';
                this.name = 'a function';
                this.simplename = 'a function';
                break;
            case 'object':
                if (isPair(v)) {
                    let tail: T = (v as Pair).snd;
                    const first = new SyntaxNode(
                        mkSyntax(((v as Pair).fst as Syntax).range, ((v as Pair).fst as Syntax).value)
                    );
                      first.parent = this;
                      this.children.push(first);
                      let i: number = 1;
                    while (isPair(tail)) {
                        const child = new SyntaxNode(
                            mkSyntax(((tail as Pair).fst as Syntax).range, ((tail as Pair).fst as Syntax).value),
                            this.children[0].simplename,
                            i
                        );
                        child.parent = this;
                        this.children.push(child);
                        i += 1;
                        tail = (tail as Pair).snd;
                      }

                    this.value = "Parenthesis List";
                    if (this.parentname != null) {
                        this.name = "the parenthesis list in argument "+this.index+" of "+this.parentname;
                    } else {
                        this.name = "the parenthesis starting with " + this.children[0].name;
                    }
                    this.simplename = 'a parenthesis list';
                } else if (isArray(v)) {
                    this.simplename = 'a square bracket array';
                    this.value = "Square bracket array";
                    this.name = "the square bracket array at "+this.syntax.range;

                    if (this.parentname != null) {
                        this.name = "the square bracket array in argument "+this.index+" of "+this.parentname;
                    }

                    let i: number = 0;
                    for (let c of (v as Vector)) {
                        const child = new SyntaxNode(mkSyntax((c as Syntax).range, (c as Syntax).value), this.name, i);
                        child.parent = this;
                        this.children.push(child);
                        i += 1;
                    }

                    if (this.parentname == null) {
                        this.name = "the square bracket array starting with " + this.children[0].name;
                    }
                } else if (isSym(v)) {
                    this.value = "Symbol " + (v as Sym).value;
                    this.name = "the symbol "+(v as Sym).value;
                    this.simplename = ''+(v as Sym).value;
                } else if (isChar(v)) {
                    this.value = "Character " + (v as Char).value;
                    this.name = "the character " + (v as Char).value;
                    this.simplename = ''+(v as Char).value;
                } else if (v === null) {
                    this.value = "null";
                    this.name = "null";
                    this.simplename = "null";
                } else {
                    this.value = "Unknown Object";
                    this.name = "an unknown object";
                    this.simplename = "unknown object";
                }
                break;
            default:
                this.value = "Unknown Value";
                this.name = "an unknown value";
                this.simplename = "unknown value";
        }
        //TODO: double check this is all of the possible syntax values (probably not)
        //this.value += " " + this.syntax.range;
    }

    toString(indent: string = ""): string {
        let ret: string = indent+this.value;

        for (let c of this.children) {
            ret += "\n" + c.toString(indent+"  ");
        }

        return ret;
    }
}

export class AST {
    syntax: Value.Syntax[]
    nodes: SyntaxNode[] = [];
    labelMap: Map<SyntaxNode, HTMLButtonElement> = new Map()

    constructor(syntax: Value.Syntax[]) {
        this.syntax = syntax;

        for (let s of this.syntax) {
            this.nodes.push(new SyntaxNode(s));
        }
    }
    
    renderNode(
        node: SyntaxNode,
        level: number,
        isLast: boolean,
        editor: EditorView,
        indexInParent: number = 0,
        totalSiblings: number = 1,
      ): HTMLElement {
        node.index = indexInParent;
        const div = document.createElement('div');
        // console.log(`Rendering "${node.value}" at level ${level} (item ${indexInParent + 1} of ${totalSiblings})`);

        const connector = isLast ? "└── " : "├── ";
        const prefix = document.createElement('span');
        prefix.textContent = `${'│   '.repeat(level - 1)}${connector}`;
        prefix.setAttribute('aria-hidden', 'true');
        prefix.setAttribute('tabindex', '-1'); // avoid accidental focus
      
        const label = document.createElement('button');
        this.labelMap.set(node, label);

        label.setAttribute(
            'aria-label',
            `${node.value}, Level ${level}, item ${indexInParent + 1} of ${totalSiblings}`
        );

        label.textContent = node.value;

        label.onclick = () => {
            console.log("Clicked " + node.syntax.range);

            editor.focus();

            editor.dispatch({
                selection: EditorSelection.create([
                    EditorSelection.range(node.syntax.range.begin.idx, node.syntax.range.end.idx+1),
                ])
            });
        }

        label.addEventListener("keydown", (e: KeyboardEvent) => {
            if (e.key === "ArrowDown") {
              if (node.children.length > 0) {
                this.labelMap.get(node.children[0])?.focus();
              }
              e.preventDefault();
            } else if (e.key === "ArrowUp") {
              if (node.parent) {
                this.labelMap.get(node.parent)?.focus();
              }
              e.preventDefault();
            } else if (e.key === "ArrowRight") {
              if (node.parent && node.index !== null && node.index + 1 < node.parent.children.length) {
                this.labelMap.get(node.parent.children[node.index + 1])?.focus();
              }
              e.preventDefault();
            } else if (e.key === "ArrowLeft") {
              if (node.parent && node.index !== null && node.index - 1 >= 0) {
                this.labelMap.get(node.parent.children[node.index - 1])?.focus();
              }
              e.preventDefault();
            }
        });
      
        div.appendChild(prefix);
        div.appendChild(label);
      
        if (node.children.length > 0) {
          const group = document.createElement('div');
          group.setAttribute('role', 'group');
          for (let i = 0; i < node.children.length; i++) {
            const child = node.children[i];
            group.appendChild(
              this.renderNode(child, level + 1, i === node.children.length - 1, editor, i, node.children.length)
            );
          }
          div.appendChild(group);
        }
        return div;
      }

    render(output: HTMLElement, editor: EditorView) {
        const container = document.createElement('div');
        container.setAttribute('id', 'ast-output');
        container.setAttribute('role', 'tree');
        const heading = document.createElement('h2');
        heading.setAttribute('aria-hidden', 'true');
        container.appendChild(heading);
        container.style.fontFamily = 'monospace';
        container.style.whiteSpace = 'pre';
      
        const dummySyntax = {
            range: { begin: { idx: 0 }, end: { idx: 0 } },
            value: null
          } as Value.Syntax;
          
        const topParent = new SyntaxNode(dummySyntax);
        topParent.children = this.nodes;
        for (let i = 0; i < this.nodes.length; i++) {
            const isLast = (i === this.nodes.length - 1);
            const node = this.nodes[i];
            node.index = i;
            node.parent = topParent;
            container.appendChild(this.renderNode(node, 1, isLast, editor, i, this.nodes.length));
        }
        output.appendChild(container);
    }


    buildTreeHTML(node: SyntaxNode): HTMLElement {
        const li = document.createElement("li");
        const div = document.createElement("div");

        div.className = "box";
        div.textContent = node.value;
        li.appendChild(div);

        if (node.children && node.children.length > 0) {
            const ul = document.createElement("ul");
            for (const child of node.children) {
                ul.appendChild(this.buildTreeHTML(child));
            }
            li.appendChild(ul);
        }

        return li;
    }

    renderTree(output: HTMLElement, nodes: SyntaxNode[]) {
        output.innerHTML = "";
        output.classList.remove("tree");

        for (const n of nodes) {
            const treeContainer = document.createElement("div");
            treeContainer.classList.add("tree");

            const rootUl = document.createElement("ul");
            rootUl.appendChild(this.buildTreeHTML(n));
            treeContainer.appendChild(rootUl);

            output.appendChild(treeContainer);
        }
    }

    describe() : string {
        if (this.nodes.length === 0) {return "The source file is empty!";}

        let queue: SyntaxNode[] = [];

        let ret: string = ""
        for (let c of this.nodes) {
            ret += "The source file contains "+c.name + ". ";
            if (c.children.length !== 0) {
                queue.push(c);
            }

            while (queue.length > 0) {
                let parent = queue.shift();
                if (parent === undefined) {
                    break;
                }
                ret += parent.name + " contains: ";
                for (let c of parent.children) {
                    ret += c.simplename + ", ";
                    if (c.children.length !== 0) {
                        queue.push(c);
                    }
                }
                ret = ret.slice(0, -2) + ". ";
            }
        }

        return ret;
    }
}