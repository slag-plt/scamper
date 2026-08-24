import * as L from '../../lpm'

// N.B., lazily instantiate AudioContext to avoid issues with non-web contexts
// TODO: need to factor appropriately so that we aren't initializing any
// web things unless we are definitely in the browser.
let ctx: AudioContext | undefined
export const audio_getCtx = (): AudioContext => {
  ctx ??= new AudioContext({ sampleRate: 16000 })
  return ctx
}

export interface SampleNode extends L.Struct {
  [L.structKind]: 'sample';
  data: Float32Array<ArrayBuffer>;
}

export function audio_sampleNode(data: number[]): SampleNode {
  for (const sample of data) {
    if (typeof sample !== 'number' || sample < -1.0 || sample > 1.0) {
      throw new L.ScamperError(
        'Runtime',
        `expected a list of numbers between -1.0 and 1.0, received ${sample.toString()}`,
      )
    }
  }
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'sample',
    data: new Float32Array(data),
  }
}

export function audio_sampleQ(v: any): boolean {
  return L.isStructKind(v, 'sample')
}

/**
 * Wraps a sample's data in a source node bound to `ctx`.
 *
 * `sample-node` yields data rather than a node: it has no context to bind to
 * at the point it is called. Anything that wants to play a sample therefore
 * converts it first, against the context it is playing into.
 *
 * N.B., the sample is duplicated into both channels, so it plays as stereo.
 */
function sampleSourceNode(
  ctx: BaseAudioContext,
  sample: SampleNode,
): AudioBufferSourceNode {
  const data = sample.data
  const buffer = ctx.createBuffer(2, data.length, ctx.sampleRate)
  buffer.copyToChannel(data, 0)
  buffer.copyToChannel(data, 1)
  const source = ctx.createBufferSource()
  source.buffer = buffer
  return source
}

export function audio_audioContext(sampleRate: number): AudioContext {
  const AudioContext = window.AudioContext
  return new AudioContext({ sampleRate })
}

export function audio_contextQ(v: any): boolean {
  return v instanceof AudioContext
}

export interface AudioPipeline extends L.Struct {
  [L.structKind]: 'audio-pipeline';
  ctx: AudioContext;
  pipeline: AudioNode;
  onOffNode: GainNode;
}

/**
 * Resolves `audio-pipeline`'s first argument, which is what the chain plays.
 *
 * A sample is data rather than a node (#181), so it becomes a source node
 * here; a node passes through. Anything else is reported as a Scamper error
 * rather than left to fail as a bare `connect is not a function`.
 */
function toSourceNode(ctx: AudioContext, v: AudioNode | SampleNode): AudioNode {
  if (audio_sampleQ(v)) {
    return sampleSourceNode(ctx, v as SampleNode)
  }
  if (v instanceof AudioNode) {
    return v
  }
  throw new L.ScamperError(
    'Runtime',
    `expected an audio node or a sample, received ${L.typeOf(v)}`,
  )
}

/**
 * Resolves one of the nodes the source is piped *through*.
 *
 * A sample cannot go here: it becomes a source node, and a source node has no
 * inputs, so connecting into it fails deep inside Web Audio with an
 * `IndexSizeError` naming neither the sample nor the pipeline. Say so instead.
 */
function toEffectNode(v: AudioNode | SampleNode): AudioNode {
  if (audio_sampleQ(v)) {
    throw new L.ScamperError(
      'Runtime',
      'a sample can only be the first argument of audio-pipeline, since it is what the pipeline plays rather than something it plays through',
    )
  }
  if (v instanceof AudioNode) {
    return v
  }
  throw new L.ScamperError(
    'Runtime',
    `expected an audio node, received ${L.typeOf(v)}`,
  )
}

export function audio_audioPipeline(
  ctx: AudioContext,
  source: AudioNode | SampleNode,
  ...rest: (AudioNode | SampleNode)[]
) {
  const pipeline = toSourceNode(ctx, source)
  const nodes = rest.map(toEffectNode)
  for (let i = 0; i < nodes.length - 1; i++) {
    nodes[i].connect(nodes[i + 1])
  }
  if (nodes.length > 0) {
    pipeline.connect(nodes[0])
  }
  const onOffNode = new GainNode(ctx)
  if (nodes.length > 0) {
    nodes[nodes.length - 1].connect(onOffNode)
  } else {
    pipeline.connect(onOffNode)
  }
  onOffNode.connect(ctx.destination)
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'audio-pipeline',
    ctx,
    pipeline,
    onOffNode,
  }
}

export function audio_pipelineQ(v: any): boolean {
  return L.isStructKind(v, 'audio-pipeline')
}

export function audio_audioNodeQ(v: any): boolean {
  return v instanceof AudioNode
}

export function audio_oscillatorNode(
  ctx: AudioContext,
  type: OscillatorType,
  freq: number,
): OscillatorNode {
  const oscillator = ctx.createOscillator()
  oscillator.type = type
  oscillator.frequency.value = freq
  return oscillator
}

// NOTE: microphone usage requires an async call! Oof! How are we suppose to
// handle that in our synchronous setting?

// async function microphoneNode (ctx: AudioContext): Promise<MediaStreamAudioSourceNode> {
//   //   const mediaStream = await navigator.mediaDevices.getUserMedia({ audio: true, video: false })
//   const source = new MediaStreamAudioSourceNode(ctx, { mediaStream })
//   return source
// }
// registerFn('microphone-node', microphoneNode, Audio)

export function audio_audioFileNode(
  ctx: AudioContext,
  filename: string,
): MediaElementAudioSourceNode {
  const mediaElement = document.createElement('audio')
  mediaElement.src = filename
  const source = new MediaElementAudioSourceNode(ctx, { mediaElement })
  return source
}

export function audio_delayNode(ctx: AudioContext, delayTime: number): DelayNode {
  return new DelayNode(ctx, { delayTime })
}

export function audio_playSample(sample: SampleNode): void {
  const ctx = audio_getCtx()
  const source = sampleSourceNode(ctx, sample)
  source.connect(ctx.destination)
  source.start()
}

