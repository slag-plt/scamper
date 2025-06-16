import { Value } from './lang.js';
export declare function mkCodeElement(text: string): HTMLElement;
export declare function mkSourceBlock(text: string): HTMLElement;
type WebRenderer = (v: any) => HTMLElement;
type TypeTest = (v: any) => boolean;
export declare function addCustomWebRenderer(test: TypeTest, renderer: WebRenderer): void;
export declare function renderToHTML(v: Value.T): HTMLElement;
export declare function renderToOutput(output: HTMLElement, v: any): void;
export {};
//# sourceMappingURL=display.d.ts.map