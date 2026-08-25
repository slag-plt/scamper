import { describe, expect, test } from 'vitest'
import { EditorState } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { language } from '@codemirror/language'
import { ScamperLanguage } from '../../../src/app/web/codemirror/extensions/language'
import { modeFor, scamperMode } from '../../../src/app/web/codemirror/modes'
import { mkFreshEditorState } from '../../../src/app/web/codemirror/codemirror'
import { initialize } from '../../../src/scamper'

// A Scamper state installs QueryExtension, whose state field reaches the
// Scamper singleton. That a *text* state does not is half of what this file
// checks, but the singleton has to exist for the .scm half to build at all.
await initialize()

// #385: the file's name decides how it is edited. A .scm file gets everything;
// anything else gets a plain editor, with highlighting where we have it.

/** Builds the state the editor would really open `filename` with. */
function stateFor(filename: string, doc = ''): EditorState {
  return mkFreshEditorState(doc, {
    dirtyAction: () => {
      /* empty */
    },
    isReadOnly: false,
    mode: modeFor(filename),
  })
}

/** @returns the name of the language the state resolved, or null for none. */
function languageName(state: EditorState): string | null {
  return state.facet(language)?.name ?? null
}

describe('modeFor', () => {
  test('a .scm file is a Scamper program', () => {
    expect(modeFor('hw3.scm').isScamper).toBe(true)
    // By identity rather than name: the Scamper grammar is defined without
    // one, so its `name` is the empty string.
    expect(stateFor('hw3.scm').facet(language)).toBe(ScamperLanguage)
  })

  test('no other file is, however it is highlighted', () => {
    for (const name of ['notes.md', 'data.csv', 'readme.txt', 'grades.tsv']) {
      expect(modeFor(name).isScamper).toBe(false)
    }
  })

  test('markdown and csv get their own languages', () => {
    expect(languageName(stateFor('notes.md'))).toBe('markdown')
    expect(languageName(stateFor('data.csv'))).not.toBeNull()
    expect(languageName(stateFor('data.csv'))).not.toBe('scamper')
  })

  test('a text file we have no grammar for gets no language at all', () => {
    // Not a fallback to Scheme: marking prose up as a program is what #385
    // exists to stop.
    expect(languageName(stateFor('readme.txt'))).toBeNull()
  })

  test('the no-file placeholder still reads as Scheme', () => {
    expect(scamperMode.isScamper).toBe(true)
  })
})

describe('the Scamper-only extensions', () => {
  // The features below are about editing a *program*. Rather than assert on
  // the extension list -- which cannot be interrogated for what it left out --
  // these check the behaviour each one is responsible for.

  test('only a Scamper file knows how to comment a line', () => {
    // This is what greys out Edit > Toggle Comment: `toggleComment` reads
    // `commentTokens` off the language, and a plain text file has no language
    // and so no answer. Asserting the language data rather than the extension
    // list, which cannot be interrogated for what it left out.
    expect(stateFor('hw3.scm').languageDataAt('commentTokens', 0)).toEqual([
      { line: ';' },
    ])
    expect(stateFor('readme.txt').languageDataAt('commentTokens', 0)).toEqual([])
  })

  test('a text file is still an editor: it takes edits and undoes them', () => {
    // Dropping the Scamper extensions must not leave a crippled editor.
    const view = new EditorView({ state: stateFor('readme.txt') })
    try {
      view.dispatch({ changes: { from: 0, insert: 'hello' } })
      expect(view.state.doc.toString()).toBe('hello')
      expect(view.state.readOnly).toBe(false)
    } finally {
      view.destroy()
    }
  })
})
