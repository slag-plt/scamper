/** One entry of a {@link PopupMenu}: an action, or a separator. */
export interface MenuItem {
  label?: string
  /** Shortcut hint shown on the right, e.g. "Alt+.". */
  kbd?: string
  run?: () => void | Promise<void>
  disabled?: boolean
  separator?: boolean
  /** Styles the item as destructive, e.g. a delete. */
  danger?: boolean
  /**
   * Renders the item as a toggle, ticked or not. Every item in a menu with any
   * checkable entry is indented to a common gutter, so their labels line up.
   */
  checked?: boolean
}
