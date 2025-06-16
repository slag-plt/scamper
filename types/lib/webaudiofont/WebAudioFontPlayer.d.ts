export declare class WebAudioFontChannel {
    constructor(audioContext: any);
    bandEqualizer: (from: any, frequency: any) => any;
}
export declare class WebAudioFontLoader {
    constructor(player: any);
    startLoad: (audioContext: any, filePath: any, variableName: any) => void;
    decodeAfterLoading: (audioContext: any, variableName: any) => void;
    waitOrFinish: (variableName: any, onFinish: any) => void;
    loaded: (variableName: any) => boolean;
    progress: () => number;
    waitLoad: (onFinish: any) => void;
    instrumentKeys: () => any;
    instrumentInfo: (n: any) => {
        variable: string;
        url: string;
        title: any;
        pitch: number;
    };
    findInstrument: (program: any) => number;
    drumTitles: () => any;
    drumKeys: () => any;
    drumInfo: (n: any) => {
        variable: string;
        url: string;
        pitch: number;
        title: any;
    };
    findDrum: (nn: any) => number;
}
export declare class WebAudioFontPlayer {
    constructor();
    createChannel: (audioContext: any) => WebAudioFontChannel;
    createReverberator: (audioContext: any) => WebAudioFontReverberator;
    limitVolume: (volume: any) => any;
    queueChord: (audioContext: any, target: any, preset: any, when: any, pitches: any, duration: any, volume: any, slides: any) => any[];
    queueStrumUp: (audioContext: any, target: any, preset: any, when: any, pitches: any, duration: any, volume: any, slides: any) => any;
    queueStrumDown: (audioContext: any, target: any, preset: any, when: any, pitches: any, duration: any, volume: any, slides: any) => any;
    queueStrum: (audioContext: any, target: any, preset: any, when: any, pitches: any, duration: any, volume: any, slides: any) => any[];
    queueSnap: (audioContext: any, target: any, preset: any, when: any, pitches: any, duration: any, volume: any, slides: any) => any;
    resumeContext: (audioContext: any) => void;
    queueWaveTable: (audioContext: any, target: any, preset: any, when: any, pitch: any, duration: any, volume: any, slides: any) => any;
    noZeroVolume: (n: any) => any;
    setupEnvelope: (audioContext: any, envelope: any, zone: any, volume: any, when: any, sampleDuration: any, noteDuration: any) => void;
    numValue: (aValue: any, defValue: any) => any;
    findEnvelope: (audioContext: any, target: any) => any;
    findZone: (audioContext: any, preset: any, pitch: any) => any;
    cancelQueue: (audioContext: any) => void;
}
/** @type class */
export declare class WebAudioFontReverberator {
    constructor(audioContext: any);
}
/** @type class */
export declare class WebAudioFontTicker {
    constructor();
    playLoop: (player: any, audioContext: any, loopStart: any, loopPosition: any, loopEnd: any, queue: any) => void;
    startTicks: (audioContext: any, onTick: any, loopStart: any, loopPosition: any, loopEnd: any, onEnd: any) => void;
    tick: (audioContext: any, nextAudioTime: any, onTick: any, loopStart: any, loopPosition: any, loopEnd: any, onEnd: any) => void;
    cancel: () => void;
}
//# sourceMappingURL=WebAudioFontPlayer.d.ts.map