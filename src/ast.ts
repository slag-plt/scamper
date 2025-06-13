import {Value} from "./lang.ts";
import {renderToOutput} from "./display.ts";
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
    children: SyntaxNode[] = [];

    constructor(syntax: Value.Syntax) {
        this.syntax = syntax;
        let v: T = this.syntax.value;

        switch (typeof v) {
            case 'boolean':
                this.value = "Boolean "+v;
                break;
            case 'number':
                this.value = "Numerical "+v;
                break;
            case 'string':
                this.value = '"'+v+'"';
                break;
            case 'undefined':
                this.value = "Undefined";
                break;
            case 'function':
                this.value = "Function";
                break;
            case 'object':
                if (isPair(v)) {
                    this.children.push(new SyntaxNode(mkSyntax(((v as Pair).fst as Syntax).range, ((v as Pair).fst as Syntax).value)))
                    let tail: T = (v as Pair).snd;

                    while (isPair(tail)) {
                        this.children.push(new SyntaxNode(mkSyntax(((tail as Pair).fst as Syntax).range, ((tail as Pair).fst as Syntax).value)));
                        tail = (tail as Pair).snd;
                    }

                    this.value = "S-expression"
                } else if (isArray(v)) {
                    this.value = "Square bracket array"

                    for (let c of (v as Vector)) {
                        this.children.push(new SyntaxNode(mkSyntax((c as Syntax).range, (c as Syntax).value)))
                    }
                } else if (isSym(v)) {
                    this.value = "Symbol " + (v as Sym).value;
                } else if (isChar(v)) {
                    this.value = "Character " + (v as Char).value;
                } else if (v === null) {
                    this.value = "null"
                } else {
                    this.value = "Unknown Object"
                }
                break;
            default:
                this.value = "Unknown Value"
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

    constructor(syntax: Value.Syntax[]) {
        this.syntax = syntax;

        for (let s of this.syntax) {
            this.nodes.push(new SyntaxNode(s));
        }
    }

    render(output: HTMLElement) {
        for (let n of this.nodes) {
            renderToOutput(output, "\n"+n.toString()+"\n");
            //renderToOutput(output, n.syntax);
        }
    }
}