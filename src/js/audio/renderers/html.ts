import * as L from '../../../lpm'
import HtmlRenderer from '../../../lpm/renderers/html.js'
import { SampleNode, AudioPipeline, audio_getCtx } from '../index.js'

function throwError(msg: string): never {
  throw new L.ScamperError('Runtime', msg)
}

export function drawOscilloscope(
  data: Uint8Array<ArrayBuffer>,
  canvas: HTMLCanvasElement,
  analyser: AnalyserNode,
) {
  requestAnimationFrame(() => {
    drawOscilloscope(data, canvas, analyser)
  })

  const bufferLength = analyser.frequencyBinCount
  analyser.getByteTimeDomainData(data)
  const ctx = canvas.getContext('2d') ?? throwError('no canvas context')
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

export function sampleRenderer(sample: SampleNode): HTMLElement {
  const ctx = audio_getCtx()
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

export interface MediaElementSource extends AudioNode {
  mediaElement: HTMLMediaElement;
}

export function isMediaElementSource(
  node: AudioNode,
): node is MediaElementSource {
  const maybeSource = node as unknown as Record<string, unknown>
  return (
    'mediaElement' in node &&
    typeof maybeSource.mediaElement === 'object' &&
    maybeSource.mediaElement !== null &&
    typeof (maybeSource.mediaElement as Record<string, unknown>).play ===
      'function'
  )
}

export function audioPipelineRenderer(blob: AudioPipeline): HTMLElement {
  const pipeline: AudioNode = blob.pipeline
  const onOffNode: GainNode = blob.onOffNode

  const ret = document.createElement('span')
  const playButton = document.createElement('button')
  playButton.textContent = '▶'
  const stopButton = document.createElement('button')
  stopButton.textContent = '■'
  const startable =
    'start' in pipeline &&
    typeof (pipeline as unknown as Record<string, unknown>).start ===
      'function'
  const isMediaElement = isMediaElementSource(pipeline)
  let started = false
  onOffNode.gain.value = 0
  playButton.onclick = () => {
    onOffNode.gain.value = 1
    if (startable && !started) {
      (pipeline as AudioScheduledSourceNode).start()
      started = true
    } else if (isMediaElement) {
      pipeline.mediaElement.play().catch((e: unknown) => {
        throw e as L.ScamperError
      })
      started = true
    }
  }
  stopButton.onclick = () => {
    onOffNode.gain.value = 0
    if (isMediaElement) {
      pipeline.mediaElement.load()
      started = false
    }
  }
  ret.appendChild(playButton)
  ret.appendChild(stopButton)
  return ret
}

HtmlRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'sample'),
  (v) => sampleRenderer(v as SampleNode),
)
HtmlRenderer.registerCustomRenderer(
  (v) => L.isStructKind(v, 'audio-pipeline'),
  (v) => audioPipelineRenderer(v as AudioPipeline),
)
