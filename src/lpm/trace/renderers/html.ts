import * as U from "../../util"
import HTMLRenderer from "../../renderers/html.js"
import { TraceStart, TraceOutput } from "../index.js"

HTMLRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, "trace-start"),
  (v) => {
    const container = document.createElement("div")
    container.classList.add("scamper-trace-start")
    const t = v as TraceStart
    container.appendChild(document.createTextNode(`${t.preamble} `))
    if (t.output) {
      container.appendChild(HTMLRenderer.render(t.output))
    }
    return container
  },
)

HTMLRenderer.registerCustomRenderer(
  (v) => U.isStructKind(v, "trace-output"),
  (v) => {
    const trace = v as TraceOutput
    const container = document.createElement("div")
    container.classList.add("scamper-trace")

    const prompt = document.createElement("code")
    prompt.textContent = "--> "
    container.appendChild(prompt)

    container.appendChild(HTMLRenderer.render(trace.output))
    return container
  },
)
