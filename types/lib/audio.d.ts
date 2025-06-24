import { Value } from '../lang.js';
import * as L from '../lang.js';
declare const Audio: L.Library;
export declare const getCtx: () => AudioContext;
interface SampleNode extends Value.Struct {
    [Value.structKind]: 'sample';
    data: Float32Array;
}
interface AudioPipeline extends Value.Struct {
    [Value.structKind]: 'audio-pipeline';
    ctx: AudioContext;
    pipeline: AudioNode;
    onOffNode: GainNode;
}
export default Audio;
export declare function sampleRenderer(sample: SampleNode): HTMLElement;
export declare function audioPipelineRenderer(blob: AudioPipeline): HTMLElement;
//# sourceMappingURL=audio.d.ts.map