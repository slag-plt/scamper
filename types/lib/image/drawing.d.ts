import { Library, Value } from '../../lang.js';
import { Rgb } from './color.js';
import { Font } from './font.js';
export declare const lib: Library;
type Mode = 'solid' | 'outline';
export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash | DText;
interface Ellipse extends Value.Struct {
    [Value.structKind]: 'ellipse';
    width: number;
    height: number;
    mode: Mode;
    color: Rgb;
}
interface Rectangle extends Value.Struct {
    [Value.structKind]: 'rectangle';
    width: number;
    height: number;
    mode: Mode;
    color: Rgb;
}
interface Triangle extends Value.Struct {
    [Value.structKind]: 'triangle';
    width: number;
    height: number;
    mode: Mode;
    color: Rgb;
}
interface Path extends Value.Struct {
    [Value.structKind]: 'path';
    width: number;
    height: number;
    points: [number, number][];
    mode: Mode;
    color: Rgb;
}
interface Beside extends Value.Struct {
    [Value.structKind]: 'beside';
    align: string;
    width: number;
    height: number;
    drawings: Drawing[];
}
interface Above extends Value.Struct {
    [Value.structKind]: 'above';
    align: string;
    width: number;
    height: number;
    drawings: Drawing[];
}
interface Overlay extends Value.Struct {
    [Value.structKind]: 'overlay';
    xAlign: string;
    yAlign: string;
    width: number;
    height: number;
    drawings: Drawing[];
}
interface OverlayOffset extends Value.Struct {
    [Value.structKind]: 'overlayOffset';
    dx: number;
    dy: number;
    width: number;
    height: number;
    d1: Drawing;
    d2: Drawing;
}
interface Rotate extends Value.Struct {
    [Value.structKind]: 'rotate';
    width: number;
    height: number;
    dx: number;
    dy: number;
    angle: number;
    drawing: Drawing;
}
interface WithDash extends Value.Struct {
    [Value.structKind]: 'withDash';
    dashSpec: number[];
    drawing: Drawing;
    width: number;
    height: number;
}
interface DText extends Value.Struct {
    [Value.structKind]: 'text';
    width: number;
    height: number;
    text: string;
    size: number;
    color: Rgb;
    font: Font;
}
/***** Rendering **************************************************************/
export declare function render(x: number, y: number, drawing: Drawing, canvas: HTMLCanvasElement): void;
export {};
//# sourceMappingURL=drawing.d.ts.map