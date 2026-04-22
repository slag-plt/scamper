declare const APP_VERSION: string

declare module "*.vue" {
  import type { DefineComponent } from "vue"
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const component: DefineComponent<unknown, unknown, any>
  export default component
}

declare module "*.css" {
  const css: string
  export default css
}

declare module "splitpanes" {
  import type { DefineComponent } from "vue"
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
