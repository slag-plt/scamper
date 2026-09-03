import { ref } from 'vue'
import {
  DEFAULT_FORMAT_MODE,
  type UserFormatMode,
} from '../../scheme/style'

/**
 * How the editor is displayed: font size, line wrapping, and how closely
 * formatting follows the rules.
 *
 * Module-level and self-persisting, in the manner of src/theme -- and for the
 * same reason. `mkFreshEditorState` builds a brand-new state every time a file
 * is opened, so anything held only in a CodeMirror compartment would be lost on
 * the next switch. Reading it from here means a fresh state starts out matching
 * what the person chose, and the live compartments only have to handle changing
 * it while a document is open.
 *
 * Kept out of the IDE's `Config` because that is saved by the app on page hide,
 * whereas these are set from a menu and should survive a crash in between.
 */

const FONT_SIZE_KEY = 'scamper.editor.fontSize'
const WORD_WRAP_KEY = 'scamper.editor.wordWrap'
const FORMAT_MODE_KEY = 'scamper.editor.formatMode'
const AUTO_SUGGEST_KEY = 'scamper.editor.autoSuggest'

/** Below this the gutter crowds the text; above it, little fits on a line. */
export const MIN_FONT_SIZE = 8
export const MAX_FONT_SIZE = 32
export const DEFAULT_FONT_SIZE = 14

const FONT_SIZE_STEP = 2

function read(key: string): string | null {
  try {
    return localStorage.getItem(key)
  } catch {
    return null // private mode, or no storage at all
  }
}

function write(key: string, value: string): void {
  try {
    localStorage.setItem(key, value)
  } catch {
    // The preference still applies for this session; remembering it is a bonus.
  }
}

function storedFontSize(): number {
  const raw = Number(read(FONT_SIZE_KEY))
  return Number.isFinite(raw) && raw >= MIN_FONT_SIZE && raw <= MAX_FONT_SIZE
    ? raw
    : DEFAULT_FONT_SIZE
}

/** The editor's font size in pixels. */
export const editorFontSize = ref<number>(storedFontSize())

/** Whether long lines wrap rather than scrolling sideways. */
export const editorWordWrap = ref<boolean>(read(WORD_WRAP_KEY) === 'true')

/**
 * Whether completions and parameter help appear on their own as you type
 * (#449). Off by default: a popup a beginner did not ask for covers the code
 * they are reading. Both are still on their keys -- Ctrl+Space and
 * Ctrl+Shift+Space -- whichever way this is set.
 */
export const autoSuggest = ref<boolean>(read(AUTO_SUGGEST_KEY) === 'true')

/**
 * How closely formatting follows the rules in docs/formatting.md: `strict` lays
 * every form out the way its rule draws it, `relaxed` keeps a `cond`/`match`
 * clause on one line while it fits. Read by the reformat command and, through
 * FormatModeKey, by the output and step panes, so a file and a trace agree.
 */
export const formatMode = ref<UserFormatMode>(
  read(FORMAT_MODE_KEY) === 'relaxed' ? 'relaxed' : DEFAULT_FORMAT_MODE,
)

/** Whether formatting is in its relaxed mode -- what the menu item shows. */
export function isRelaxedFormatting(): boolean {
  return formatMode.value === 'relaxed'
}

export function setFormatMode(mode: UserFormatMode): void {
  formatMode.value = mode
  write(FORMAT_MODE_KEY, mode)
}

export function toggleRelaxedFormatting(): void {
  setFormatMode(formatMode.value === 'relaxed' ? 'strict' : 'relaxed')
}

export function setEditorFontSize(size: number): void {
  const clamped = Math.min(MAX_FONT_SIZE, Math.max(MIN_FONT_SIZE, size))
  editorFontSize.value = clamped
  write(FONT_SIZE_KEY, String(clamped))
}

export function zoomIn(): void {
  setEditorFontSize(editorFontSize.value + FONT_SIZE_STEP)
}

export function zoomOut(): void {
  setEditorFontSize(editorFontSize.value - FONT_SIZE_STEP)
}

export function resetZoom(): void {
  setEditorFontSize(DEFAULT_FONT_SIZE)
}

export function setEditorWordWrap(on: boolean): void {
  editorWordWrap.value = on
  write(WORD_WRAP_KEY, String(on))
}

export function toggleEditorWordWrap(): void {
  setEditorWordWrap(!editorWordWrap.value)
}

export function setAutoSuggest(on: boolean): void {
  autoSuggest.value = on
  write(AUTO_SUGGEST_KEY, String(on))
}

export function toggleAutoSuggest(): void {
  setAutoSuggest(!autoSuggest.value)
}
