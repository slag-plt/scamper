import { ScamperError, Value } from '../lang.js'
import * as L from '../lang.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'
import * as Display from '../display.js'

const Audio: L.Library = L.emptyLibrary()

// N.B., lazily instantiate AudioContext to avoid issues with non-web contexts
// TODO: need to factor appropriately so that we aren't initializing any
// web things unless we are definitely in the browser.
let ctx: AudioContext | undefined
export const getCtx = (): AudioContext => {
  if (ctx === undefined) {
    ctx = new AudioContext({ sampleRate: 16000 })
  }
  return ctx
}

interface SampleNode extends Value.Struct { [Value.structKind]: 'sample', data: Float32Array }

function sampleNode (data: number[]): SampleNode {
  checkContract(arguments, contract('sample-node', [C.vector]))
  for (let i = 0; i < data.length; i++) {
    if (typeof data[i] !== 'number' || data[i] as number < -1.0 || data[i] as number > 1.0) {
      throw new ScamperError('Runtime', `expected a list of numbers between -1.0 and 1.0, received ${data[i]}`)
    }
  }
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'sample', data: new Float32Array(data) }
}
L.registerValue('sample-node', sampleNode, Audio)

function audioContext (sampleRate: number): AudioContext {
  checkContract(arguments, contract('audio-context', [C.integer]))
  const AudioContext = window.AudioContext
  return new AudioContext({ sampleRate })
}
L.registerValue('audio-context', audioContext, Audio)

interface AudioPipeline extends Value.Struct {
  [Value.structKind]: 'audio-pipeline',
  ctx: AudioContext,
  pipeline: AudioNode,
  onOffNode: GainNode
}

function audioPipeline (ctx: AudioContext, pipeline: AudioNode, ...nodes: AudioNode[]) {
  // TODO: need to check types on the anys... but they're JS types!
  checkContract(arguments, contract('audio-pipeline', [C.any, C.any], C.any))
  for (let i = 0; i < nodes.length - 1; i++) {
    nodes[i].connect(nodes[i + 1])
  }
  if (nodes.length > 0) {
    pipeline.connect(nodes[0])
  }
  const onOffNode = new GainNode(ctx);
  (nodes[arguments.length - 1] as AudioNode).connect(onOffNode)
  onOffNode.connect(ctx.destination)
  return { [Value.scamperTag]: 'struct', [Value.structKind]: 'audio-pipeline', ctx, pipeline, onOffNode }
}
L.registerValue('audio-pipeline', audioPipeline, Audio)

function oscillatorNode (ctx: AudioContext, type: OscillatorType, freq: number): OscillatorNode {
  checkContract(arguments, contract('oscillator-node', [C.any, C.string, C.integer]))
  const oscillator = ctx.createOscillator()
  oscillator.type = type
  oscillator.frequency.value = freq
  return oscillator
}
L.registerValue('oscillator-node', oscillatorNode, Audio)

// NOTE: microphone usage requires an async call! Oof! How are we suppose to
// handle that in our synchronous setting?

// async function microphoneNode (ctx: AudioContext): Promise<MediaStreamAudioSourceNode> {
//   checkContract(arguments, contract('microphone-node', [C.any]))
//   const mediaStream = await navigator.mediaDevices.getUserMedia({ audio: true, video: false })
//   const source = new MediaStreamAudioSourceNode(ctx, { mediaStream })
//   return source
// }
// registerFn('microphone-node', microphoneNode, Audio)

function audioFileNode (ctx: AudioContext, filename: string): MediaElementAudioSourceNode {
  checkContract(arguments, contract('audio-file-node', [C.any, C.string]))
  const mediaElement = document.createElement('audio')
  mediaElement.src = filename
  const source = new MediaElementAudioSourceNode(ctx, { mediaElement })
  return source
}
L.registerValue('audio-file-node', audioFileNode, Audio)

function delayNode (ctx: AudioContext, delayTime: number): DelayNode {
  checkContract(arguments, contract('delay-node', [C.any, C.integer]))
  return new DelayNode(ctx, { delayTime })
}
L.registerValue('delay-node', delayNode, Audio)

function playSample (pipeline: SampleNode): void {
  checkContract(arguments, contract('play-sample', [C.any]))
  if (L.Value.isStructKind(pipeline, 'sample')) {
    const ctx = getCtx()
    const data = pipeline.data
    // N.B., for now, make the audio sample stereo (2 channels)
    const buffer = ctx.createBuffer(2, data.length, ctx.sampleRate)
    buffer.copyToChannel(data, 0)
    buffer.copyToChannel(data, 1)
    const source = ctx.createBufferSource()
    source.buffer = buffer
    source.connect(ctx.destination)
    source.start()
  } else {
    throw new ScamperError('Runtime', `expected a sample node, received ${pipeline}`)
  }
}
L.registerValue('play-sample', playSample, Audio)

export default Audio

///// Audio Rendering //////////////////////////////////////////////////////////

function drawOscilloscope (data: Uint8Array, canvas: HTMLCanvasElement, analyser: AnalyserNode) {
  requestAnimationFrame(() => drawOscilloscope(data, canvas, analyser))

  const bufferLength = analyser.frequencyBinCount
  analyser.getByteTimeDomainData(data)
  const ctx = canvas.getContext('2d')!
  ctx.fillStyle = 'rgb(200, 200, 200)'
  ctx.fillRect(0, 0, canvas.width, canvas.height)

  ctx.lineWidth = 2
  ctx.strokeStyle = 'rgb(0, 0, 0)'
  ctx.beginPath()
  const sliceWidth = (canvas.width * 1.0) / bufferLength
  let x = 0

  for (let i = 0; i < bufferLength; i++) {
    const v = data[i] / 128.0
    const y = (v * canvas.height) / 2

    if (i === 0) {
      ctx.moveTo(x, y)
    } else {
      ctx.lineTo(x, y)
    }

    x += sliceWidth
  }

  ctx.lineTo(canvas.width, canvas.height / 2)
  ctx.stroke()
}

export function sampleRenderer (sample: SampleNode): HTMLElement {
  const ctx = getCtx()
  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  const visualizer = document.createElement('canvas')

  const analyser = ctx.createAnalyser()
  analyser.fftSize = 2048
  const bufferLength = analyser.frequencyBinCount
  const dataArray = new Uint8Array(bufferLength)
  analyser.getByteTimeDomainData(dataArray)
  
  const data = sample.data
  // N.B., for now, make the audio sample stereo (2 channels)
  const buffer = ctx.createBuffer(2, data.length, ctx.sampleRate)
  buffer.copyToChannel(data, 0)
  buffer.copyToChannel(data, 1)
  let source: AudioBufferSourceNode | undefined
  playButton.onclick = () => {
    source = ctx.createBufferSource()
    source.buffer = buffer
    source.connect(ctx.destination)
    source.connect(analyser)
    source.start()
    drawOscilloscope(dataArray, visualizer, analyser)
  }
  stopButton.onclick = () => {
    if (source !== undefined) {
      source.stop()
    }
  }

  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  ret.appendChild(visualizer)
  return ret
}

export function audioPipelineRenderer (blob: AudioPipeline): HTMLElement {
  const pipeline: AudioScheduledSourceNode = blob.pipeline as AudioScheduledSourceNode
  const onOffNode: GainNode = blob.onOffNode

  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  const startable = typeof (pipeline as any).start !== 'undefined'
  const sourceIsFile = typeof (pipeline as any).mediaElement !== 'undefined' && typeof (pipeline as any).mediaElement.play !== 'undefined'
  let started = false
  onOffNode.gain.value = 0
  playButton.onclick = _ => {
    onOffNode.gain.value = 1
    if (startable && !started) {
      pipeline.start()
      started = true
    } else if (sourceIsFile) {
      (pipeline as any).mediaElement.play()
      started = true
    }
  }
  stopButton.onclick = _ => {
    onOffNode.gain.value = 0
    if (sourceIsFile) {
      (pipeline as any).mediaElement.load()
      started = false
    }
  }
  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  return ret
}

Display.addCustomWebRenderer((v) => L.Value.isStructKind(v, 'sample'), sampleRenderer)
Display.addCustomWebRenderer((v) => L.Value.isStructKind(v, 'audio-pipeline'), audioPipelineRenderer)