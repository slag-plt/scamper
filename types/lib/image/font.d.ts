import * as C from '../../contract.js';
import { Library, Value } from '../../lang.js';
export declare const lib: Library;
export interface Font extends Value.Struct {
    [Value.structKind]: 'font';
    face: string;
    system: string;
    isBold: boolean;
    isItalic: boolean;
}
export declare const fontS: C.Spec;
export declare function fontToFontString(f: Font, size: number): string;
export declare function font(name: string, system?: string, isBold?: boolean, isItalic?: boolean): Font;
//# sourceMappingURL=font.d.ts.map