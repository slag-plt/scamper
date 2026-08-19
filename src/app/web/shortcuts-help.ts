import { ref } from 'vue'

/**
 * Whether the keyboard-shortcut cheatsheet is showing.
 *
 * Module-level rather than internal to the component so the Help menu can open
 * the panel, which lives over in the header next to the (?) that also opens it.
 * The alternative was threading a template ref up through two components for a
 * single boolean.
 */
export const shortcutsHelpOpen = ref(false)
