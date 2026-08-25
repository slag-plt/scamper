import type { InjectionKey, Ref } from 'vue'
import type { UserFormatMode } from '../style'

/**
 * How closely the web backend follows the formatting rules -- the same choice
 * the reformat command makes, so a trace and a file are laid out alike.
 *
 * Provided rather than passed as a prop for the reason ChangedPathKey is: the
 * layout is reached through the generic value renderer, and threading a setting
 * for one caller through all of that would put formatting knowledge into
 * components that have no business with it.
 *
 * Absent outside the IDE -- in the widget, in a test -- where the default in
 * src/scheme/style.ts applies.
 */
export const FormatModeKey: InjectionKey<Ref<UserFormatMode>> =
  Symbol('FormatMode')
