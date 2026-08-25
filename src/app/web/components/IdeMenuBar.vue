<script setup lang="ts">
import { computed, onMounted, onUnmounted, ref } from 'vue'
import PopupMenu from './PopupMenu.vue'
import type { MenuItem } from '../menu'
import { useScamperSession } from '../composables/use-scamper-session'
import { useEditor } from '../composables/editor-context'
import type { CodeMirrorEditorAdapter } from '../composables/codemirror-editor-adapter'
import { appShortcut, editShortcut, isMac } from '../edit-commands'
import { shortcutsHelpOpen } from '../shortcuts-help'
import {
  showSourceWithOutput,
  toggleShowSourceWithOutput,
} from '../output-prefs'
import { showHiddenFiles, toggleShowHiddenFiles } from '../file-prefs'
import {
  checkExamples,
  liveEvaluation,
  toggleCheckExamples,
  toggleLiveEvaluation,
} from '../run-prefs'
import {
  DEFAULT_FONT_SIZE,
  MAX_FONT_SIZE,
  MIN_FONT_SIZE,
  editorFontSize,
  editorWordWrap,
  isRelaxedFormatting,
  resetZoom,
  toggleEditorWordWrap,
  toggleRelaxedFormatting,
  zoomIn,
  zoomOut,
} from '../editor-prefs'
import { currentTheme, toggleTheme } from '../../../theme'

/**
 * The IDE's menu bar: File / Edit / Go / Run / View / Help, in the manner of a
 * desktop editor.
 *
 * The menus are the complete list of what the IDE can do. The toolbar below
 * keeps the few actions worth a single click (run, trace, the sidebar), so a
 * handful of things deliberately appear in both places -- the menus are for
 * finding an action, the toolbar for repeating one.
 *
 * File operations arrive as props because they belong to the app that owns the
 * file system; run and edit commands are taken straight off the session and the
 * editor, which are provided to the whole tree.
 */
const props = defineProps<{
  currentFile?: string | null
  /** Whether this deployment has a file server, and whether one can sign in. */
  hasServer?: boolean
  canSignIn?: boolean
  signedInAs?: string | null
  /** Required, unlike the callbacks: the View menu ticks the item from it. */
  isSidebarVisible: boolean
  /** Files to offer under "Recent", newest first, already filtered. */
  recentFiles?: string[]
  create?: () => void
  upload?: () => void
  save?: () => void
  saveAs?: () => void
  closeFile?: () => void
  rename?: (filename: string) => void
  deleteFile?: (filename: string) => void
  download?: (filename: string) => void
  archive?: () => void
  history?: (filename?: string) => void
  selectFile?: (filename: string) => void
  signIn?: () => void
  signOut?: () => void
  /** One entry per panel that exists, for the View menu's placement section. */
  panelPlacement?: { label: string; floating: boolean; toggle: () => void }[]
  /** Shown at the left end of the bar, e.g. "(3.5.0)". */
  version?: string
  toggleSidebar?: () => void
  /** Whether the cursor is inside a statement, and whether one is being traced. */
  canStep?: boolean
  isStepping?: boolean
  stepStatement?: () => void
  about?: () => void
  whatsNew?: () => void
}>()

const session = useScamperSession()
const editor = useEditor()

/** The editor adapter, or null before it has registered itself. */
function adapter(): CodeMirrorEditorAdapter | null {
  try {
    return editor()
  } catch {
    return null
  }
}

/**
 * Runs `command` on the editor and puts the cursor back in it -- a menu pick
 * moves focus to the menu, and typing after "Paste" should land in the code.
 */
function inEditor(command: (ed: CodeMirrorEditorAdapter) => void) {
  return () => {
    const ed = adapter()
    if (ed === null) return
    command(ed)
    ed.focus()
  }
}

// Recomputed every time a menu is opened (see `openMenu`), so what is greyed
// out reflects the moment of opening rather than whenever the bar was built.
const editorStatus = ref({
  loaded: false,
  readOnly: true,
  hasSelection: false,
  isScamper: false,
  canUndo: false,
  canRedo: false,
  onIdentifier: false,
})

function refreshEditorStatus() {
  const ed = adapter()
  editorStatus.value =
    ed === null
      ? {
          loaded: false,
          readOnly: true,
          hasSelection: false,
          isScamper: false,
          canUndo: false,
          canRedo: false,
          onIdentifier: false,
        }
      : { loaded: ed.isLoaded(), ...ed.status() }
}

const isRunning = computed(() => session.currentRun.value !== null)

/** Opens `url` in its own tab, the way the toolbar's links do. */
function openPage(url: string) {
  window.open(url, '_blank')
}

const fileMenu = computed<MenuItem[]>(() => {
  const file = props.currentFile ?? null
  const items: MenuItem[] = [
    { label: 'New File…', run: () => props.create?.() },
    { label: 'Upload File…', run: () => props.upload?.() },
    { separator: true },
    { label: 'Save', kbd: appShortcut.save, disabled: file === null, run: () => props.save?.() },
    { label: 'Save As…', disabled: file === null, run: () => props.saveAs?.() },
    {
      label: 'Rename…',
      disabled: file === null,
      run: () => {
        if (file !== null) props.rename?.(file)
      },
    },
    {
      label: 'Download',
      disabled: file === null,
      run: () => {
        if (file !== null) props.download?.(file)
      },
    },
    {
      label: 'Delete',
      danger: true,
      disabled: file === null,
      run: () => {
        if (file !== null) props.deleteFile?.(file)
      },
    },
    {
      label: 'Close File',
      disabled: file === null,
      run: () => props.closeFile?.(),
    },
    { separator: true },
    { label: 'File History…', run: () => props.history?.() },
    { label: 'Export All as Zip…', run: () => props.archive?.() },
  ]
  // Flat rather than the submenu a desktop editor would use, since PopupMenu
  // has one level. At five entries a section reads fine; a submenu would be
  // more machinery than the list is worth.
  const recent = props.recentFiles ?? []
  if (recent.length > 0) {
    items.push({ separator: true })
    for (const name of recent) {
      items.push({ label: name, run: () => props.selectFile?.(name) })
    }
  }
  // A deployment with no server has no accounts to sign in to, and the dev
  // stub has a server but no way to sign in.
  if (props.hasServer && props.canSignIn) {
    const who = props.signedInAs ?? null
    items.push({ separator: true })
    items.push(
      who === null
        ? { label: 'Sign In…', run: () => props.signIn?.() }
        : { label: `Sign Out (${who})`, run: () => props.signOut?.() },
    )
  }
  return items
})

const editMenu = computed<MenuItem[]>(() => {
  const s = editorStatus.value
  return [
    { label: 'Undo', kbd: editShortcut.undo, disabled: !s.canUndo, run: inEditor((ed) => { ed.undo() }) },
    { label: 'Redo', kbd: editShortcut.redo, disabled: !s.canRedo, run: inEditor((ed) => { ed.redo() }) },
    { separator: true },
    { label: 'Cut', kbd: editShortcut.cut, disabled: s.readOnly || !s.hasSelection, run: inEditor((ed) => { void ed.cut() }) },
    { label: 'Copy', kbd: editShortcut.copy, disabled: !s.hasSelection, run: inEditor((ed) => { ed.copy() }) },
    { label: 'Paste', kbd: editShortcut.paste, disabled: s.readOnly, run: inEditor((ed) => { ed.paste() }) },
    { label: 'Select All', kbd: editShortcut.selectAll, disabled: !s.loaded, run: inEditor((ed) => { ed.selectAll() }) },
    { separator: true },
    { label: 'Find…', kbd: editShortcut.find, disabled: !s.loaded, run: inEditor((ed) => { ed.find() }) },
    { label: 'Replace…', disabled: s.readOnly, run: inEditor((ed) => { ed.replace() }) },
    { separator: true },
    { label: 'Toggle Comment', kbd: editShortcut.toggleComment, disabled: s.readOnly || !s.isScamper, run: inEditor((ed) => { ed.toggleComment() }) },
    { label: 'Format File', kbd: editShortcut.format, disabled: s.readOnly || !s.isScamper, run: inEditor((ed) => { ed.format() }) },
    // Sits under Format File because that is the command it changes -- though
    // it governs the output and step panes too, so a file and a trace agree.
    {
      label: 'Relaxed Formatting',
      checked: isRelaxedFormatting(),
      run: () => { toggleRelaxedFormatting() },
    },
  ]
})

const goMenu = computed<MenuItem[]>(() => {
  const s = editorStatus.value
  return [
    { label: 'Go to Line…', kbd: editShortcut.goToLine, disabled: !s.loaded, run: inEditor((ed) => { ed.goToLine() }) },
    { separator: true },
    { label: 'Go to Definition', kbd: editShortcut.goToDefinition, disabled: !s.onIdentifier || !s.isScamper, run: inEditor((ed) => { ed.goToDefinition() }) },
    { label: 'Find References', kbd: editShortcut.findReferences, disabled: !s.onIdentifier || !s.isScamper, run: inEditor((ed) => { ed.findReferences() }) },
  ]
})

const runMenu = computed<MenuItem[]>(() => [
  // The access key is still there, but it is not what gets named: every browser
  // and platform invokes one with a different chord, so a hint for it would be
  // wrong for most people reading it. Mod+Enter is the same on all of them.
  { label: 'Run', kbd: appShortcut.run, disabled: isRunning.value, run: () => session.execute() },
  { label: 'Stop', disabled: !isRunning.value, run: () => { session.stopRun() } },
  // Stop-then-run: what a student means by "restart" is a clean run of the
  // code as it stands, not a resumption of the one in flight.
  {
    label: 'Restart',
    disabled: !isRunning.value,
    run: () => {
      session.stopRun()
      return session.execute()
    },
  },
  { separator: true },
  // A toggle rather than a command, and it belongs here rather than in View:
  // it decides *when* the program runs, not how anything is displayed.
  {
    label: 'Live Evaluation',
    checked: liveEvaluation.value,
    run: () => { toggleLiveEvaluation() },
  },
  // Its own toggle rather than part of Live Evaluation: checking examples
  // re-runs the file once per example, which is a cost worth being able to
  // decline on its own (issue #374).
  {
    label: 'Check Examples',
    checked: checkExamples.value,
    run: () => { toggleCheckExamples() },
  },
  { separator: true },
  // Stepping is per-statement now, in its own window; the old whole-program
  // trace is what the output pane's own step controls still drive.
  {
    label: 'Step Statement at Cursor…',
    disabled: !props.canStep || props.isStepping,
    run: () => props.stepStatement?.(),
  },
  { separator: true },
  { label: 'Query Value at Cursor', run: () => session.query() },
])

const viewMenu = computed<MenuItem[]>(() => {
  const s = editorStatus.value
  return [
    {
      label: 'File Drawer',
      checked: props.isSidebarVisible,
      run: () => props.toggleSidebar?.(),
    },
    {
      label: 'Show Hidden Files',
      checked: showHiddenFiles.value,
      run: () => { toggleShowHiddenFiles() },
    },
    { separator: true },
    {
      label: 'Zoom In',
      disabled: editorFontSize.value >= MAX_FONT_SIZE,
      run: () => { zoomIn() },
    },
    {
      label: 'Zoom Out',
      disabled: editorFontSize.value <= MIN_FONT_SIZE,
      run: () => { zoomOut() },
    },
    {
      label: 'Reset Zoom',
      disabled: editorFontSize.value === DEFAULT_FONT_SIZE,
      run: () => { resetZoom() },
    },
    { separator: true },
    {
      label: 'Word Wrap',
      checked: editorWordWrap.value,
      run: () => { toggleEditorWordWrap() },
    },
    {
      label: 'Source with Output',
      checked: showSourceWithOutput.value,
      run: () => { toggleShowSourceWithOutput() },
    },
    { separator: true },
    { label: 'Fold All', kbd: editShortcut.foldAll, disabled: !s.loaded, run: inEditor((ed) => { ed.foldAll() }) },
    { label: 'Unfold All', kbd: editShortcut.unfoldAll, disabled: !s.loaded, run: inEditor((ed) => { ed.unfoldAll() }) },
    { separator: true },
    // Float/Dock for each panel. The tab strips and title bars carry the same
    // commands, but a panel alone in its slot has neither -- both of them, in
    // the default arrangement -- so this is the one surface that can always
    // reach all of them.
    ...(props.panelPlacement ?? []).map((panel) => ({
      label: `${panel.floating ? 'Dock' : 'Float'} ${panel.label}`,
      run: () => { panel.toggle() },
    })),
    { separator: true },
    {
      label: 'Dark Theme',
      checked: currentTheme.value === 'dark',
      run: () => { toggleTheme() },
    },
  ]
})

const helpMenu = computed<MenuItem[]>(() => [
  { label: 'Documentation', run: () => { openPage('docs.html') } },
  { label: 'Language Reference', run: () => { openPage('reference.html') } },
  { label: 'Search Functions', run: () => { openPage('search.html') } },
  { separator: true },
  { label: 'Keyboard Shortcuts', run: () => { shortcutsHelpOpen.value = true } },
  { separator: true },
  { label: 'Scamper on GitHub', run: () => { openPage('https://github.com/slag-plt/scamper') } },
  { label: 'Report an Issue', run: () => { openPage('https://github.com/slag-plt/scamper/issues') } },
  { separator: true },
  { label: "What's New", run: () => props.whatsNew?.() },
  { label: 'About Scamper', run: () => props.about?.() },
])

const menus = computed(() => [
  { title: 'File', items: fileMenu.value },
  { title: 'Edit', items: editMenu.value },
  { title: 'Go', items: goMenu.value },
  { title: 'Run', items: runMenu.value },
  { title: 'View', items: viewMenu.value },
  { title: 'Help', items: helpMenu.value },
])

// The open menu's title, and where to draw its panel.
const barRef = ref<HTMLElement | null>(null)
const openTitle = ref<string | null>(null)
/** Set when the open came from the keyboard; see PopupMenu's autoActivate. */
const openedByKeyboard = ref(false)
const menuPos = ref({ x: 0, y: 0 })
const openItems = computed(
  () => menus.value.find((m) => m.title === openTitle.value)?.items ?? [],
)

function openMenu(title: string, event: MouseEvent) {
  openedByKeyboard.value = false
  refreshEditorStatus()
  const rect = (event.currentTarget as HTMLElement).getBoundingClientRect()
  menuPos.value = { x: rect.left, y: rect.bottom }
  openTitle.value = title
}

function toggleMenu(title: string, event: MouseEvent) {
  if (openTitle.value === title) {
    openTitle.value = null
    return
  }
  openMenu(title, event)
}

/**
 * Once a menu is open, sliding along the bar opens each menu in turn without
 * clicking again -- the behaviour every desktop menu bar has.
 */
function onTitleEnter(title: string, event: MouseEvent) {
  if (openTitle.value !== null && openTitle.value !== title) {
    openMenu(title, event)
  }
}

/** Opens `title`'s menu, positioned under its button in the bar. */
function openMenuByTitle(title: string, focusButton: boolean) {
  const button = barRef.value?.querySelector<HTMLElement>(
    `[data-menu="${title}"]`,
  )
  if (!button) return
  if (focusButton) button.focus()
  refreshEditorStatus()
  const rect = button.getBoundingClientRect()
  menuPos.value = { x: rect.left, y: rect.bottom }
  openTitle.value = title
}

/**
 * Which title the bar's single tab stop is on.
 *
 * A menubar is one stop, not six: Tab moves past the whole bar and the arrow
 * keys move within it. Before this every title was its own stop, so tabbing
 * from the sidebar to the editor went through all six.
 */
const focusedTitle = ref<string | null>(null)

const tabStop = computed(
  () => focusedTitle.value ?? openTitle.value ?? menus.value[0]?.title ?? null,
)

/** Moves the bar's focus to `title`, opening its menu if one is already open. */
function goToTitle(title: string) {
  focusedTitle.value = title
  if (openTitle.value !== null) {
    openMenuByTitle(title, true)
  } else {
    barRef.value?.querySelector<HTMLElement>(`[data-menu="${title}"]`)?.focus()
  }
}

/**
 * The menubar keyboard pattern: Left/Right along the bar, Home/End to its ends,
 * and Down to open the focused menu. Only the first of these existed, and only
 * once a menu was already open -- so from the keyboard there was no way in.
 */
function onTitleKey(event: KeyboardEvent) {
  const titles = menus.value.map((m) => m.title)
  if (titles.length === 0) return
  const at = Math.max(0, titles.indexOf(tabStop.value ?? titles[0]))

  switch (event.key) {
    case 'ArrowRight':
      event.preventDefault()
      goToTitle(titles[(at + 1) % titles.length])
      return
    case 'ArrowLeft':
      event.preventDefault()
      goToTitle(titles[(at - 1 + titles.length) % titles.length])
      return
    case 'Home':
      event.preventDefault()
      goToTitle(titles[0])
      return
    case 'End':
      event.preventDefault()
      goToTitle(titles[titles.length - 1])
      return
    case 'ArrowDown':
    case 'Enter':
    case ' ':
      if (openTitle.value === null) {
        event.preventDefault()
        // Stopped here rather than allowed to bubble on: the menu mounts at the
        // microtask checkpoint between this listener and the one on document,
        // so the very same keypress would otherwise reach the fresh menu and
        // step it a second time. autoActivate does that job explicitly.
        event.stopPropagation()
        openedByKeyboard.value = true
        openMenuByTitle(titles[at], false)
      }
      return
    default:
      return
  }
}

/**
 * Alt+F for File, and so on down the bar -- the accelerators a desktop menu
 * bar has. Not on macOS, where Option is how you type é and ° rather than a
 * menu modifier, and where no native application binds it this way.
 */
function onAcceleratorKey(event: KeyboardEvent) {
  if (isMac || !event.altKey || event.ctrlKey || event.metaKey) return
  const hit = menus.value.find(
    (m) => m.title[0].toLowerCase() === event.key.toLowerCase(),
  )
  if (!hit) return
  event.preventDefault()
  if (openTitle.value === hit.title) {
    openTitle.value = null
  } else {
    openMenuByTitle(hit.title, true)
  }
}

// Underlining the accelerator letter is the visual half of the convention, and
// like a desktop menu bar it only appears while Alt is down.
const altHeld = ref(false)

function onWindowKeyDown(event: KeyboardEvent) {
  if (!isMac && event.key === 'Alt') altHeld.value = true
  onAcceleratorKey(event)
}

function onWindowKeyUp(event: KeyboardEvent) {
  if (event.key === 'Alt') altHeld.value = false
}

// Alt+Tab and the like take focus away mid-press, so the key-up never arrives.
function onWindowBlur() {
  altHeld.value = false
}

onMounted(() => {
  window.addEventListener('keydown', onWindowKeyDown)
  window.addEventListener('keyup', onWindowKeyUp)
  window.addEventListener('blur', onWindowBlur)
})

onUnmounted(() => {
  window.removeEventListener('keydown', onWindowKeyDown)
  window.removeEventListener('keyup', onWindowKeyUp)
  window.removeEventListener('blur', onWindowBlur)
})
</script>

<template>
  <div ref="barRef" class="ide-menu-bar" @keydown="onTitleKey">
    <!-- Outside the menubar role: it is a wordmark, not a command, and a
         menubar's children have to be menuitems. It used to head the file
         drawer, where it cost a whole row of a narrow column to say something
         that never changes. -->
    <span class="brand">
      Scamper<span v-if="version" class="brand-version">{{ version }}</span>
    </span>
    <div class="menu-titles" role="menubar">
    <button
      v-for="menu in menus"
      :key="menu.title"
      type="button"
      role="menuitem"
      class="menu-title"
      :class="{ open: openTitle === menu.title }"
      :data-menu="menu.title"
      aria-haspopup="menu"
      :aria-expanded="openTitle === menu.title"
      :tabindex="tabStop === menu.title ? 0 : -1"
      @mousedown.stop
      @focus="focusedTitle = menu.title"
      @click="toggleMenu(menu.title, $event)"
      @mouseenter="onTitleEnter(menu.title, $event)"
    ><span :class="{ accelerator: altHeld }">{{ menu.title[0] }}</span
      >{{ menu.title.slice(1) }}</button>
    </div>
    <PopupMenu
      v-if="openTitle !== null"
      :x="menuPos.x"
      :y="menuPos.y"
      :items="openItems"
      :auto-activate="openedByKeyboard"
      @close="openTitle = null"
    />
  </div>
</template>

<style scoped>
.ide-menu-bar {
  display: flex;
  align-items: center;
  gap: var(--space-sm);
  padding: 0 var(--space-sm);
  background: var(--header-bg);
  color: var(--header-fg);
  border-bottom: 1px solid var(--border);
  /* Above the editor, so an open menu is not clipped by it. */
  z-index: var(--z-menubar);
  user-select: none;
}

.menu-titles {
  display: flex;
  align-items: center;
  gap: 0.1em;
}

.brand {
  flex-shrink: 0;
  padding-inline: var(--space-xs);
  font-size: var(--text-md);
  font-weight: 600;
  color: var(--fg);
  white-space: nowrap;
  user-select: none;
}

.brand-version {
  /* A margin rather than a literal space: Vue's compiler condenses whitespace
     between the text and the span away. */
  margin-inline-start: 0.35em;
  font-weight: 400;
  font-variant-numeric: tabular-nums;
  opacity: 0.6;
}

.menu-title {
  border: none;
  background: none;
  padding: var(--space-xs) var(--space-lg);
  font: inherit;
  font-size: var(--text-md);
  color: inherit;
  border-radius: var(--radius-md);
  cursor: pointer;
}

.menu-title:hover,
.menu-title.open {
  background: var(--surface-hover);
}

.accelerator {
  text-decoration: underline;
}
</style>
