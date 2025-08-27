import * as C from '../contract.js';
import * as L from '../../lpm';
export declare const lib: L.Library;
export interface Font extends L.Struct {
    [L.structKind]: 'font';
    face: string;
    system: string;
    isBold: boolean;
    isItalic: boolean;
}
export declare const fontS: C.Spec;
export declare function fontToFontString(f: Font, size: number): string;
export declare function font(name: string, system?: string, isBold?: boolean, isItalic?: boolean): Font;
//# sourceMappingURL=font.d.ts.map