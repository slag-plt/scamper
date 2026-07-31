import { ref } from 'vue'

/**
 * Light/dark theme state, shared across every Scamper app.
 *
 * The source of truth for CSS is the `data-theme` attribute on <html> plus the
 * `light-dark()` tokens in public/css/theme.css. This module keeps a reactive
 * mirror for Vue components and resolves tokens for non-CSS surfaces (canvas,
 * Chart.js). A no-flash inline <head> script applies the persisted choice
 * before first paint; this module takes over once the app boots.
 *
 * N.B., this module is transitively imported by JS renderers that also load in
 * non-browser environments (the Node CLI, jsdom tests), so every access to
 * window / document / localStorage is guarded.
 */

export type Theme = 'light' | 'dark'

const STORAGE_KEY = 'scamper-theme'

const hasWindow = typeof window !== 'undefined'
const hasDocument = typeof document !== 'undefined'

function systemPrefersDark(): boolean {
  if (!hasWindow || !window.matchMedia) {
    return false
  }
  return window.matchMedia('(prefers-color-scheme: dark)').matches
}

/** The user's explicit override, if any. */
function storedTheme(): Theme | null {
  try {
    const v = localStorage.getItem(STORAGE_KEY)
    return v === 'light' || v === 'dark' ? v : null
  } catch {
    return null
  }
}

/** The theme currently in effect: an explicit override, else the OS default. */
export function effectiveTheme(): Theme {
  return storedTheme() ?? (systemPrefersDark() ? 'dark' : 'light')
}

/** Reactive current theme, for Vue components (e.g. the toggle icon). */
export const currentTheme = ref<Theme>(effectiveTheme())

/** Applies and persists an explicit theme choice. */
export function setTheme(theme: Theme): void {
  try {
    localStorage.setItem(STORAGE_KEY, theme)
  } catch {
    // Ignore storage failures (e.g. private mode); the in-page theme still applies.
  }
  if (hasDocument) {
    document.documentElement.setAttribute('data-theme', theme)
  }
  currentTheme.value = theme
  if (hasWindow) {
    window.dispatchEvent(new CustomEvent<Theme>('themechange', { detail: theme }))
  }
}

/** Flips between light and dark. */
export function toggleTheme(): void {
  setTheme(currentTheme.value === 'dark' ? 'light' : 'dark')
}

/**
 * Resolves a CSS color token (including `light-dark()`) to a concrete color
 * string, for canvas / Chart.js which cannot consume CSS variables directly.
 * @param name a custom-property name, e.g. '--canvas-surface'
 * @returns a resolved color string, or '' when no DOM is available.
 */
export function readColorToken(name: string): string {
  if (!hasDocument) {
    return ''
  }
  const probe = document.createElement('span')
  probe.style.color = `var(${name})`
  probe.style.display = 'none'
  document.documentElement.appendChild(probe)
  const color = getComputedStyle(probe).color
  probe.remove()
  return color
}

/**
 * Subscribes to theme changes (for non-Vue renderers that must repaint, e.g.
 * canvas drawings and Chart.js). Fires on both explicit toggles and OS changes.
 * @returns an unsubscribe function (a no-op when there is no window).
 */
export function onThemeChange(cb: (theme: Theme) => void): () => void {
  if (!hasWindow) {
    return () => {}
  }
  const handler = (e: Event) => cb((e as CustomEvent<Theme>).detail)
  window.addEventListener('themechange', handler)
  return () => window.removeEventListener('themechange', handler)
}

// Track OS changes while running: when the user hasn't set an explicit override,
// follow the system preference live (CSS updates via color-scheme; this keeps
// the Vue mirror and canvas renderers in sync).
if (hasWindow && window.matchMedia) {
  window
    .matchMedia('(prefers-color-scheme: dark)')
    .addEventListener('change', (e) => {
      if (storedTheme() !== null) {
        return
      }
      const theme: Theme = e.matches ? 'dark' : 'light'
      currentTheme.value = theme
      window.dispatchEvent(
        new CustomEvent<Theme>('themechange', { detail: theme }),
      )
    })
}
