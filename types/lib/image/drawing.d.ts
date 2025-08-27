import * as L from '../../lpm';
import { Rgb } from './color.js';
import { Font } from './font.js';
export declare const lib: L.Library;
type Mode = 'solid' | 'outline';
export type Drawing = Ellipse | Rectangle | Triangle | Path | Beside | Above | Overlay | OverlayOffset | Rotate | WithDash | DText;
interface Ellipse extends L.Struct {
    [L.structKind]: 'ellipse';
    width: number;
    height: number;
    mode: Mode;
    color: Rgb;
}
interface Rectangle extends L.Struct {
    [L.structKind]: 'rectangle';
    width: number;
    height: number;
    mode: Mode;
    color: Rgb;
}
interface Triangle extends L.Struct {
    [L.structKind]: 'triangle';
    width: number;
    height: number;
    mode: Mode;
    color: Rgb;
}
interface Path extends L.Struct {
    [L.structKind]: 'path';
    width: number;
    height: number;
    points: [number, number][];
    mode: Mode;
    color: Rgb;
}
interface Beside extends L.Struct {
    [L.structKind]: 'beside';
    align: string;
    width: number;
    height: number;
    drawings: Drawing[];
}
interface Above extends L.Struct {
    [L.structKind]: 'above';
    align: string;
    width: number;
    height: number;
    drawings: Drawing[];
}
interface Overlay extends L.Struct {
    [L.structKind]: 'overlay';
    xAlign: string;
    yAlign: string;
    width: number;
    height: number;
    drawings: Drawing[];
}
interface OverlayOffset extends L.Struct {
    [L.structKind]: 'overlayOffset';
    dx: number;
    dy: number;
    width: number;
    height: number;
    d1: Drawing;
    d2: Drawing;
}
interface Rotate extends L.Struct {
    [L.structKind]: 'rotate';
    width: number;
    height: number;
    dx: number;
    dy: number;
    angle: number;
    drawing: Drawing;
}
interface WithDash extends L.Struct {
    [L.structKind]: 'withDash';
    dashSpec: number[];
    drawing: Drawing;
    width: number;
    height: number;
}
interface DText extends L.Struct {
    [L.structKind]: 'text';
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