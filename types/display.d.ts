import * as LPM from './lpm';
export declare function mkCodeElement(text: string): HTMLElement;
export declare function mkSourceBlock(text: string): HTMLElement;
type WebRenderer = (v: any) => HTMLElement;
type TypeTest = (v: any) => boolean;
export declare function addCustomWebRenderer(test: TypeTest, renderer: WebRenderer): void;
export declare function renderToHTML(v: LPM.Value): HTMLElement;
export declare function renderToOutput(output: HTMLElement, v: any): void;
export declare class HTMLDisplay implements LPM.OutputChannel, LPM.ErrorChannel {
    display: HTMLElement;
    constructor(display: HTMLElement);
    send(v: LPM.Value): void;
    report(err: LPM.ScamperError): void;
}
export {};
//# sourceMappingURL=display.d.ts.map