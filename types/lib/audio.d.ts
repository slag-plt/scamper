import * as R from '../lpm/runtime.js';
declare const Audio: R.Library;
export declare const getCtx: () => AudioContext;
interface SampleNode extends R.Struct {
    [R.structKind]: 'sample';
    data: Float32Array;
}
interface AudioPipeline extends R.Struct {
    [R.structKind]: 'audio-pipeline';
    ctx: AudioContext;
    pipeline: AudioNode;
    onOffNode: GainNode;
}
export default Audio;
export declare function sampleRenderer(sample: SampleNode): HTMLElement;
export declare function audioPipelineRenderer(blob: AudioPipeline): HTMLElement;
//# sourceMappingURL=audio.d.ts.map