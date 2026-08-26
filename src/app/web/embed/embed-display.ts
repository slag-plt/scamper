import { HTMLDisplay } from '../../../lpm/output/html'
import { highlightScamper } from '../source-highlight'

/**
 * An output channel that captions each statement's output with the statement
 * itself, so a reading widget reads as a transcript: code, then what it
 * produced, then the next piece of code (#375).
 *
 * The scheduler calls `beginStatement` once per source form of a captioned run
 * -- once even for a form that produces no output, and once however many values
 * it produces -- which is exactly the interleaving a transcript wants.
 */
export class EmbedDisplay extends HTMLDisplay {
  /** Appends `source` as a highlighted code block at the current level. */
  beginStatement(source: string): void {
    const block = document.createElement('div')
    block.classList.add('scamper-transcript-source')
    const code = document.createElement('code')
    // Built with createElement and textContent rather than innerHTML: the text
    // is the author's source, and nothing in it should ever become markup.
    for (const token of highlightScamper(source)) {
      const span = document.createElement('span')
      if (token.cls !== null) span.className = token.cls
      span.textContent = token.text
      code.appendChild(span)
    }
    block.appendChild(code)
    this.levels[this.levels.length - 1].appendChild(block)
  }
}
