declare class Player {
    fontName: string;
    player: any;
    audioContext: AudioContext;
    loadInstrument(instr: number, isPercussion?: boolean): void;
    constructor();
    getInstrument(id: number, isPercussion?: boolean): any;
}
export declare function waf(): Player;
export {};
//# sourceMappingURL=webaudiofont.d.ts.map