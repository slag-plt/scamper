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

class SyntaxNode {
    syntax: Value.Syntax
    value: string = ''
    name: string = ''
    parentname: string | null = null;
    simplename: string = '';
    index: number | null = null;
    children: SyntaxNode[] = [];

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
                    this.children.push(new SyntaxNode(mkSyntax(((v as Pair).fst as Syntax).range, ((v as Pair).fst as Syntax).value)));
                    let tail: T = (v as Pair).snd;

                    if (isPair(this.children[0].syntax.value)) {
                        while (isPair(tail)) {
                            this.children.push(new SyntaxNode(mkSyntax(((tail as Pair).fst as Syntax).range, ((tail as Pair).fst as Syntax).value)));
                            tail = (tail as Pair).snd;
                        }
                    } else {
                        let i: number = 1;
                        while (isPair(tail)) {
                            this.children.push(new SyntaxNode(mkSyntax(((tail as Pair).fst as Syntax).range, ((tail as Pair).fst as Syntax).value), this.children[0].simplename, i));
                            i += 1;
                            tail = (tail as Pair).snd;
                        }
                    }

                    this.value = "S-expression";
                    if (this.parentname != null) {
                        this.name = "the s-expression in argument "+this.index+" of "+this.parentname;
                    } else {
                        this.name = "the s-expression starting with " + this.children[0].name;
                    }
                    this.simplename = 'an s-expression';
                } else if (isArray(v)) {
                    this.simplename = 'a square bracket array';
                    this.value = "Square bracket array";
                    this.name = "the square bracket array at "+this.syntax.range;

                    if (this.parentname != null) {
                        this.name = "the square bracket array in argument "+this.index+" of "+this.parentname;
                    }

                    let i: number = 0;
                    for (let c of (v as Vector)) {
                        this.children.push(new SyntaxNode(mkSyntax((c as Syntax).range, (c as Syntax).value),this.name,i));
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

    toAsciiTree(prefix: string = "", isLast: boolean = true): string {
        const connector = isLast ? "└── " : "├── "
        let result = prefix + connector + this.value + "\n"
        const newPrefix = prefix + (isLast ? "    " : "│   ")
      
        for (let i = 0; i < this.children.length; i++) {
          result += this.children[i].toAsciiTree(newPrefix, i === this.children.length - 1)
        }
        return result
    }
}

export class AST {
    syntax: Value.Syntax[]
    nodes: SyntaxNode[] = [];

    constructor(syntax: Value.Syntax[]) {
        this.syntax = syntax;

        for (let s of this.syntax) {
            this.nodes.push(new SyntaxNode(s));
        }
    }

    render(output: HTMLElement) {
        const pre = document.createElement('pre')
        pre.setAttribute('id', 'ast-output')
        pre.setAttribute('aria-label', 'Abstract Syntax Tree')
        pre.setAttribute('role', 'tree')
        pre.style.fontFamily = 'monospace'
        for (let i = 0; i < this.nodes.length; i++) {
            const isLast = (i === this.nodes.length - 1)
            pre.textContent += this.nodes[i].toAsciiTree("", isLast)
        }
        output.appendChild(pre)
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