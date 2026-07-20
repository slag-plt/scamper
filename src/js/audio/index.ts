import * as L from "../../lpm";
import { checkContract, contract } from "../contract.js";
import * as C from "../contract.js";

// N.B., lazily instantiate AudioContext to avoid issues with non-web contexts
// TODO: need to factor appropriately so that we aren't initializing any
// web things unless we are definitely in the browser.
let ctx: AudioContext | undefined;
export const audio_getCtx = (): AudioContext => {
  ctx ??= new AudioContext({ sampleRate: 16000 });
  return ctx;
};

export interface SampleNode extends L.Struct {
  [L.structKind]: "sample";
  data: Float32Array<ArrayBuffer>;
}

export function audio_sampleNode(data: number[]): SampleNode {
  checkContract(arguments, contract("sample-node", [C.vector]));
  for (const sample of data) {
    if (typeof sample !== "number" || sample < -1.0 || sample > 1.0) {
      throw new L.ScamperError(
        "Runtime",
        `expected a list of numbers between -1.0 and 1.0, received ${sample.toString()}`,
      );
    }
  }
  return {
    [L.scamperTag]: "struct",
    [L.structKind]: "sample",
    data: new Float32Array(data),
  };
}

export function audio_audioContext(sampleRate: number): AudioContext {
  checkContract(arguments, contract("audio-context", [C.integer]));
  const AudioContext = window.AudioContext;
  return new AudioContext({ sampleRate });
}

export interface AudioPipeline extends L.Struct {
  [L.structKind]: "audio-pipeline";
  ctx: AudioContext;
  pipeline: AudioNode;
  onOffNode: GainNode;
}

export function audio_audioPipeline(
  ctx: AudioContext,
  pipeline: AudioNode,
  ...nodes: AudioNode[]
) {
  // TODO: need to check types on the anys... but they're JS types!
  checkContract(arguments, contract("audio-pipeline", [C.any, C.any], C.any));
  for (let i = 0; i < nodes.length - 1; i++) {
    nodes[i].connect(nodes[i + 1]);
  }
  if (nodes.length > 0) {
    pipeline.connect(nodes[0]);
  }
  const onOffNode = new GainNode(ctx);
  if (nodes.length > 0) {
    nodes[nodes.length - 1].connect(onOffNode);
  } else {
    pipeline.connect(onOffNode);
  }
  onOffNode.connect(ctx.destination);
  return {
    [L.scamperTag]: "struct",
    [L.structKind]: "audio-pipeline",
    ctx,
    pipeline,
    onOffNode,
  };
}

export function audio_oscillatorNode(
  ctx: AudioContext,
  type: OscillatorType,
  freq: number,
): OscillatorNode {
  checkContract(
    arguments,
    contract("oscillator-node", [C.any, C.string, C.integer]),
  );
  const oscillator = ctx.createOscillator();
  oscillator.type = type;
  oscillator.frequency.value = freq;
  return oscillator;
}

// NOTE: microphone usage requires an async call! Oof! How are we suppose to
// handle that in our synchronous setting?

// async function microphoneNode (ctx: AudioContext): Promise<MediaStreamAudioSourceNode> {
//   checkContract(arguments, contract('microphone-node', [C.any]))
//   const mediaStream = await navigator.mediaDevices.getUserMedia({ audio: true, video: false })
//   const source = new MediaStreamAudioSourceNode(ctx, { mediaStream })
//   return source
// }
// registerFn('microphone-node', microphoneNode, Audio)

export function audio_audioFileNode(
  ctx: AudioContext,
  filename: string,
): MediaElementAudioSourceNode {
  checkContract(arguments, contract("audio-file-node", [C.any, C.string]));
  const mediaElement = document.createElement("audio");
  mediaElement.src = filename;
  const source = new MediaElementAudioSourceNode(ctx, { mediaElement });
  return source;
}

export function audio_delayNode(ctx: AudioContext, delayTime: number): DelayNode {
  checkContract(arguments, contract("delay-node", [C.any, C.integer]));
  return new DelayNode(ctx, { delayTime });
}

export function audio_playSample(pipeline: SampleNode): void {
  checkContract(arguments, contract("play-sample", [C.any]));
  // TODO: this should never happen, when does it happen?
  // if (!L.isStructKind(pipeline, "sample")) {
  //   throw new L.ScamperError(
  //     "Runtime",
  //     `expected a sample node, received ${pipeline}`,
  //   );
  // }
  const ctx = audio_getCtx();
  // TODO: error message function should be in some util file instead of inlined
  const data = pipeline.data;
  // N.B., for now, make the audio sample stereo (2 channels)
  const buffer = ctx.createBuffer(2, data.length, ctx.sampleRate);
  buffer.copyToChannel(data, 0);
  buffer.copyToChannel(data, 1);
  const source = ctx.createBufferSource();
  source.buffer = buffer;
  source.connect(ctx.destination);
  source.start();
}

