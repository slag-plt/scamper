/**
 * The keyboard shortcuts the editor binds, as labels to show next to the menu
 * item that runs the same command.
 *
 * These mirror CodeMirror's keymaps rather than defining them -- the bindings
 * live in `codemirror/codemirror.ts` and in CodeMirror's own defaults. Kept in
 * one place so the menu bar, the right-click menu, and the shortcut cheatsheet
 * can't drift from each other on which modifier this platform uses.
 */

export const isMac = /Mac|iPhone|iPad|iPod/i.test(
  typeof navigator === 'undefined' ? '' : navigator.userAgent,
)

/** The platform's primary modifier, which CodeMirror's keymaps call "Mod". */
export const mod = isMac ? 'Cmd' : 'Ctrl'

export const editShortcut = {
  undo: `${mod}+Z`,
  redo: `${mod}+Shift+Z`,
  cut: `${mod}+X`,
  copy: `${mod}+C`,
  paste: `${mod}+V`,
  selectAll: `${mod}+A`,
  find: `${mod}+F`,
  toggleComment: `${mod}+/`,
  format: `${mod}+Shift+I`,
  goToLine: `${mod}+Alt+G`,
  goToDefinition: 'Alt+.',
  findReferences: 'Shift+Alt+.',
  // @codemirror/language binds the fold-everything pair the same way on every
  // platform, unlike the single-line fold above it.
  foldAll: 'Ctrl+Alt+[',
  unfoldAll: 'Ctrl+Alt+]',
} as const
