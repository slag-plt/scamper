import * as L from '../lpm';
declare const Audio: L.Library;
export declare const getCtx: () => AudioContext;
interface SampleNode extends L.Struct {
    [L.structKind]: 'sample';
    data: Float32Array;
}
interface AudioPipeline extends L.Struct {
    [L.structKind]: 'audio-pipeline';
    ctx: AudioContext;
    pipeline: AudioNode;
    onOffNode: GainNode;
}
export default Audio;
export declare function sampleRenderer(sample: SampleNode): HTMLElement;
export declare function audioPipelineRenderer(blob: AudioPipeline): HTMLElement;
//# sourceMappingURL=audio.d.ts.map