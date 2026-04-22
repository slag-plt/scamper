import { EditorView } from "@codemirror/view"

import OPFSFileSystem from "./fs.js"
import Split from "split.js"
import { renderToOutput } from "../lpm/output/html.js"
import * as Lock from "./lockfile.js"
import { mkFreshEditorState, mkNoFileEditorState } from "./codemirror.js"
import { initializeLibs } from "../lib/index.js"
import ScamperVue from "../scamper-vue"
import { throwNull } from "../util"
import { createApp } from "vue"
import ResultsPane from "./ResultsPane.vue"
import type { ResultsPaneType } from "./use-results-pane"
import IdeHeader from "./IdeHeader.vue"
import type { IdeHeaderType } from "./use-ide-header"
import IdeSidebar from "./IdeSidebar.vue"
import type { IdeSidebarType } from "./use-ide-sidebar"

const editorPane = document.getElementById("editor")!
const resultsPaneEl =
  document.getElementById("results") ?? throwNull("no results pane?")

const configFilename = ".scamper.config"

interface Config {
  lastOpenedFilename: string | null
  lastVersionAccessed: string
}

const defaultConfig: Config = {
  lastOpenedFilename: null,
  lastVersionAccessed: "0.0.0",
}


class IDE {
  fs: OPFSFileSystem
  editor: EditorView
  private _currentFile: string | null = null
  headerComponent: IdeHeaderType | null = null
  sidebarComponent: IdeSidebarType | null = null
  autosaveId: number
  scamper?: ScamperVue
  isDirty: boolean
  config: Config
  isLoadingFile = false
  // N.B., showDotFiles should normally be false except when are debugging
  showDotFiles = false
  resultsComponent: ResultsPaneType

  get currentFile(): string | null {
    return this._currentFile
  }

  set currentFile(v: string | null) {
    this._currentFile = v
    this.headerComponent?.setCurrentFile(v)
    this.sidebarComponent?.setCurrentFile(v)
  }

  ///// Initialization /////////////////////////////////////////////////////////

  private constructor(fs: OPFSFileSystem) {
    this.fs = fs
    this.config = defaultConfig // N.B., loaded asynchronously from disk in IDE.create()
    this.autosaveId = -1
    this.isDirty = false

    const resultsApp = createApp(ResultsPane, {
      stepOnce: () => {
        this.scamper?.stepProgram()
        this.resultsComponent.scrollToBottom()
      },
      stepStmt: async () => {
        await this.scamper?.stepStmtProgram()
        this.resultsComponent.scrollToBottom()
      },
      stepAll: async () => {
        await this.scamper?.runProgram()
        this.resultsComponent.scrollToBottom()
      },
      astText: () => {
        this.showASTText()
      },
      cancel: () => {
        this.scamper?.cancel()
      },
    })
    this.resultsComponent = resultsApp.mount(
      resultsPaneEl,
    ) as unknown as ResultsPaneType

    const headerEl =
      document.getElementById("header") ?? throwNull("no header element?")
    const headerApp = createApp(IdeHeader, {
      run: async () => {
        this.startScamper(false)
        await this.scamper?.runProgram()
      },
      trace: () => {
        this.startScamper(true)
      },
      cancel: () => {
        this.scamper?.cancel()
      },
      onRunWindow: async () => {
        if (!this.currentFile) return
        await this.saveCurrentFile()
        const params = new URLSearchParams({
          filename: this.currentFile,
          isTree: "false",
        })
        window.open(`runner.html?${params.toString()}`)
      },
      onToggleSidebar: () => {
        const sidebar = document.getElementById("sidebar")
        if (!sidebar) return
        const isVisible = sidebar.style.display !== "none"
        sidebar.style.display = isVisible ? "none" : "block"
      },
    })
    this.headerComponent = headerApp.mount(headerEl) as unknown as IdeHeaderType

    const sidebarEl =
      document.getElementById("sidebar") ?? throwNull("no sidebar element?")
    const sidebarApp = createApp(IdeSidebar, {
      version: `(${APP_VERSION})`,
      create: async () => {
        const filename = prompt("Enter a file name for your new program.")
        if (filename !== null) {
          if (await this.fs.fileExists(filename)) {
            alert(`File ${filename} already exists!`)
          } else {
            await this.fs.saveFile(filename, `; ${filename}`)
            await this.switchToFile(filename)
          }
        }
      },
      uploadFile: async (file: File) => {
        const content = await file.text()
        const filename = file.name
        if (await this.fs.fileExists(filename)) {
          const shouldOverwrite = confirm(
            `File "${filename}" already exists. Do you want to overwrite it?`,
          )
          if (!shouldOverwrite) return
          this.stopAutosaving()
          await this.fs.deleteFile(filename)
        }
        await this.fs.saveFile(filename, content)
        this.currentFile = null
        await this.switchToFile(filename)
      },
      fileDrop: async (droppedFiles: FileList) => {
        this.stopAutosaving()
        for (const file of droppedFiles) {
          try {
            const content = await file.text()
            const filename = file.name
            if (await this.fs.fileExists(filename)) {
              const shouldOverwrite = confirm(
                `File "${filename}" already exists. Do you want to overwrite it?`,
              )
              if (!shouldOverwrite) continue
              this.stopAutosaving()
              await this.fs.deleteFile(filename)
            }
            await this.fs.saveFile(filename, content)
            this.currentFile = null
            await this.switchToFile(filename)
          } catch (e) {
            if (e instanceof Error) {
              this.displayError(
                `Failed to upload file "${file.name}": ${e.message}`,
              )
            }
          }
        }
      },
      rename: async () => {
        if (!this.currentFile) return
        const newName = prompt(`Enter a new filename for ${this.currentFile}`)
        if (newName !== null && newName !== this.currentFile) {
          if (await this.fs.fileExists(newName)) {
            alert(`File ${newName} already exists!`)
          } else {
            try {
              this.stopAutosaving()
              // N.B., in renaming the file, the this.fs webworker will close
              // its handle to the current file, so we should be able to
              // just load it as if no file was open.
              await this.fs.renameFile(this.currentFile, newName)
              this.currentFile = null
              await this.switchToFile(newName)
            } catch (e) {
              if (e instanceof Error) {
                this.displayError(e.message)
              }
            }
          }
        }
      },
      deleteFile: async () => {
        if (!this.currentFile) return
        const shouldDelete = confirm(
          `Are you sure you want to delete ${this.currentFile}?`,
        )
        if (shouldDelete) {
          this.stopAutosaving()
          await this.fs.deleteFile(this.currentFile)
          this.currentFile = null
          this.initializeDummyDoc()
          this.config.lastOpenedFilename = null
          this.resultsComponent.reset()
          await this.populateFileDrawer()
          this.startAutosaving()
        }
      },
      download: async () => {
        if (!this.currentFile) return
        const contents = await this.fs.loadFile(this.currentFile)
        const hidden = document.createElement("a")
        hidden.href =
          "data:attachment/text;charset=utf-8," + encodeURIComponent(contents)
        hidden.target = "_blank"
        hidden.download = this.currentFile
        hidden.click()
      },
      selectFile: async (filename: string) => {
        if (!this.isLoadingFile) {
          await this.switchToFile(filename)
        }
      },
    })
    this.sidebarComponent = sidebarApp.mount(sidebarEl) as unknown as IdeSidebarType

    Split(["#editor", "#results"], { sizes: [65, 35] })
    this.editor = new EditorView({
      state: mkNoFileEditorState(),
      parent: editorPane,
    })

    // N.B., this section is wonky. I'm trying to set up hooks to save on exit
    // but nothing is consistent! I think we eventually want to use just
    // visibilitychange and beforeunload. It seems like beforeunload is the
    // most consistent at the cost of killing bfcache... whicih I don't mind
    // for this particular application.
    document.addEventListener("visibilitychange", async () => {
      if (document.visibilityState === "hidden") {
        await this.saveCurrentFile()
        await this.saveConfig()
        await Lock.releaseLockFile(this.fs)
      } else if (document.visibilityState === "visible") {
        await Lock.acquireLockFile(this.fs)
      }
    })

    document.addEventListener("pagehide", async () => {
      await this.saveCurrentFile()
      await this.saveConfig()
      await Lock.releaseLockFile(this.fs)
    })

    window.addEventListener("beforeunload", async (e) => {
      // N.B., ensure the "are you sure" dialog pops
      await this.saveCurrentFile()
      await this.saveConfig()
      await Lock.releaseLockFile(this.fs)
      if (this.isDirty) {
        e.preventDefault()
        return true
      } else {
        return false
      }
    })

  }


  /** Populates the file drawer with entries. */
  private async populateFileDrawer() {
    if (!this.fs) {
      throw new Error("FileChooser: must call init() before usage")
    }
    const allFiles = await this.fs.getFileList()
    const visible = allFiles.filter(
      (f) => !f.isDirectory && (this.showDotFiles || !f.name.startsWith(".")),
    )
    this.sidebarComponent?.setFiles(visible)
  }

  /** Saves the current configuration to disk */
  async saveConfig() {
    await this.fs.saveFile(configFilename, JSON.stringify(this.config))
  }

  /** Loads the configuration from disk */
  async loadConfig() {
    if (await this.fs.fileExists(configFilename)) {
      this.config = JSON.parse(await this.fs.loadFile(configFilename))
    } else {
      this.config = defaultConfig
      await this.saveConfig()
    }
  }

  static async create() {
    const fs = await OPFSFileSystem.create()
    await initializeLibs()
    const obtainedLock = await Lock.acquireLockFile(fs)
    if (!obtainedLock) {
      document.getElementById("loading-content")!.innerText =
        "Another instance of Scamper is open. Please close that instance and try again."
      document.getElementById("loading")!.style.display = "block"
    } else {
      const ide = new IDE(fs)
      await ide.populateFileDrawer()
      await ide.loadConfig()
      if (ide.config.lastOpenedFilename !== null) {
        if (await fs.fileExists(ide.config.lastOpenedFilename)) {
          // TODO: re-enable once we have a better handle on large-file loading
          // await ide.switchToFile(ide.config.lastOpenedFilename)
        } else {
          ide.config.lastOpenedFilename = null
        }
      }
    }
  }

  ///// IDE utility functions //////////////////////////////////////////////////

  startAutosaving() {
    if (this.autosaveId === -1) {
      this.autosaveId = window.setInterval(async () => {
        await this.saveCurrentFile()
      }, 3000)
    }
  }

  stopAutosaving() {
    window.clearInterval(this.autosaveId)
    this.autosaveId = -1
  }

  getDoc(): string {
    return this.editor.state.doc.toString()
  }

  initializeDoc(src: string) {
    this.editor.setState(
      mkFreshEditorState(src, {
        dirtyAction: () => {
          this.makeDirty()
        },
        isReadOnly: false,
      }),
    )
  }

  initializeDummyDoc() {
    this.editor.setState(mkNoFileEditorState())
  }

  makeDirty() {
    if (this.scamper !== undefined && !this.isDirty) {
      this.isDirty = true
      this.resultsComponent.makeDirty()
    }
  }

  makeClean() {
    this.resultsComponent.makeClean()
    this.isDirty = false
  }

  ///// IDE actions ////////////////////////////////////////////////////////////

  startScamper(isTracing: boolean): void {
    this.resultsComponent.reset()
    try {
      this.scamper = new ScamperVue(
        this.resultsComponent.display,
        this.getDoc(),
        isTracing,
      )
      this.resultsComponent.setTracing(isTracing)
    } catch (e) {
      renderToOutput(resultsPaneEl, e)
    }
    this.makeClean()
  }

  showASTText(): void {
    const existing = document.getElementById("ast-output")
    const label = document.getElementById("ast-label")
    const desc = document.getElementById("ast-desc")
    if (existing) {
      existing.remove()
      if (label) {
        label.remove()
      }
      if (desc) {
        desc.remove()
      }
      return
    }
    try {
      // TODO: reimplement with new backend!
    } catch (e) {
      renderToOutput(resultsPaneEl, e)
    }
  }

  async saveCurrentFile() {
    if (!this.currentFile) {
      return
    }
    try {
      await this.fs.saveFile(this.currentFile, this.getDoc())
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(e.message)
      }
    }
  }

  async switchToFile(filename: string): Promise<void> {
    this.isLoadingFile = true
    if (!this.fs) return
    // Stop autosaving to make the transaction atomic
    this.stopAutosaving()

    // Save the current file and close our handle to it, if open
    if (this.currentFile !== null) {
      await this.saveCurrentFile()
    }

    // Load the file!
    this.currentFile = filename
    try {
      const src = await this.fs.loadFile(this.currentFile)
      this.initializeDoc(src)
    } catch (e) {
      if (e instanceof Error) {
        this.displayError(`${e.message}\n\n${e.stack}`)
      }
    }

    // Reset the UI: output panel and file drawer, also restart saving
    this.resultsComponent.reset()
    await this.populateFileDrawer()
    this.config.lastOpenedFilename = this.currentFile
    this.startAutosaving()
    this.isLoadingFile = false
  }

  displayError(error: string) {
    document.getElementById("loading-content")!.innerText = error
    document.getElementById("loading")!.style.display = "block"
  }
}

export const ide = await IDE.create()
