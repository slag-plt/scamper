import * as C from '../../contract.js';
import { Library, Value } from '../../lang.js';
/** Converts between various representations of color in Scamper. */
export declare function colorToRgb(v: any): Rgb;
export declare function colorQ(v: any): boolean;
export declare const colorS: C.Spec;
/***** RGB(A) Colors **********************************************************/
export interface Rgb extends Value.Struct {
    [Value.structKind]: 'rgba';
    red: number;
    green: number;
    blue: number;
    alpha: number;
}
export declare function isRgb(v: any): boolean;
export declare function rgb(...args: number[]): Rgb;
export declare function rgbToString(rgba: Rgb): string;
/***** RGB hex strings ********************************************************/
/***** HSV colors *************************************************************/
interface Hsv extends Value.Struct {
    [Value.structKind]: 'hsv';
    hue: number;
    saturation: number;
    value: number;
    alpha: number;
}
/***** Other predicates *******************************************************/
/***** Color conversion *******************************************************/
export declare function colorNameToRgb(name: string): Rgb;
export declare function hsvToRgb(hsv: Hsv): Rgb;
export declare function rgbAverage(rgba1: Rgb, rgba2: Rgb): Rgb;
/***** Exports ****************************************************************/
export declare const lib: Library;
export {};
//# sourceMappingURL=color.d.ts.map