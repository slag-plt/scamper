declare const APP_VERSION: string

/**
 * True only in `vite --mode server` (`npm run dev:full`), which runs the front
 * end against the back end in `server/`. A build-time constant, so a production
 * bundle drops the branches it guards -- see src/app/web/dev-backend.ts.
 */
declare const SCAMPER_DEV_SERVER: boolean

declare module '*.vue' {
  import type { DefineComponent } from 'vue'
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const component: DefineComponent<unknown, unknown, any>
  export default component
}

declare module '*.css' {
  const css: string
  export default css
}

declare module '*.scm?raw' {
  const src: string
  export default src
}

declare module 'splitpanes' {
  import type { DefineComponent } from 'vue'
  export const Splitpanes: DefineComponent<{
    horizontal?: boolean
    pushOtherPanes?: boolean
    maximizePanes?: boolean
    rtl?: boolean
    firstSplitter?: boolean
  }>
  export const Pane: DefineComponent<{
    size?: number
    minSize?: number
    maxSize?: number
  }>
}
