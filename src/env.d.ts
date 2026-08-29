declare const APP_VERSION: string

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

// Vite builds the module and inlines it as a blob URL; the default export
// constructs a worker running it. See src/fs/opfs-writer.ts.
declare module '*?worker&inline' {
  const workerConstructor: new () => Worker
  export default workerConstructor
}
