import { EditorView } from "@codemirror/view"

import OPFSFileSystem from "./fs.js"
import Split from "split.js"
import { Scamper } from "../scamper.js"
import { renderToOutput } from "../lpm/output/html.js"
import * as Lock from "./lockfile.js"
import { mkFreshEditorState, mkNoFileEditorState } from "./codemirror.js"
import { initializeLibs } from "../lib/index.js"
import { mkInProgress } from "./in-progress"

const editorPane = document.getElementById("editor")
const outputPane = document.getElementById("output")

if (editorPane === null || outputPane === null) {
  throw new Error("editor or output pane not found")
}

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
  currentFile: string | null
  autosaveId: number
  scamper?: Scamper
  isDirty: boolean
  config: Config
  isLoadingFile = false
  // N.B., showDotFiles should normally be false except when are debugging
  showDotFiles = false

  ///// Initialization /////////////////////////////////////////////////////////

  private constructor(fs: OPFSFileSystem) {
    this.fs = fs
    this.config = defaultConfig // N.B., loaded asynchronously from disk in IDE.create()
    this.currentFile = null
    this.autosaveId = -1
    this.isDirty = false
    this.fs = fs

    Split(["#editor", "#results"], { sizes: [65, 35] })
    this.editor = new EditorView({
      state: mkNoFileEditorState(outputPane),
      parent: editorPane,
    })

    const version = document.getElementById("version")
    if (version === null) {
      throw new Error("version element not found")
    }
    version.innerText = `(${APP_VERSION})`

    // N.B., this section is wonky. I'm trying to set up hooks to save on exit
    // but nothing is consistent! I think we eventually want to use just
    // visibilitychange and beforeunload. It seems like beforeunload is the
    // most consistent at the cost of killing bfcache... whicih I don't mind
    // for this particular application.
    document.addEventListener("visibilitychange", () => {
      void (async () => {
        if (document.visibilityState === "hidden") {
          await this.saveCurrentFile()
          await this.saveConfig()
          await Lock.releaseLockFile(this.fs)
        } else {
          await Lock.acquireLockFile(this.fs)
        }
      })()
    })

    document.addEventListener("pagehide", () => {
      void (async () => {
        await this.saveCurrentFile()
        await this.saveConfig()
        await Lock.releaseLockFile(this.fs)
      })()
    })

    window.addEventListener("beforeunload", (e) => {
      void (async () => {
        await this.saveCurrentFile()
        await this.saveConfig()
        await Lock.releaseLockFile(this.fs)
      })()
    
      if (this.isDirty) {
        e.preventDefault()
      }
    })

    this.initFileDropZone()
    this.initButtons()
  }

  /** Initializes the sidebar's dropzone */
  private initFileDropZone() {
    const sidebar = document.getElementById("sidebar")
    if (sidebar === null) {
      throw new Error("sidebar not found")
    }

    sidebar.addEventListener("dragover", (event) => {
      event.preventDefault()
      if (event.dataTransfer !== null) {
        event.dataTransfer.dropEffect = "copy"
      }
      sidebar.classList.add("drag-over")
    })

    sidebar.addEventListener("dragleave", (event) => {
      event.preventDefault()
      sidebar.classList.remove("drag-over")
    })

    sidebar.addEventListener("drop", (event) => {
      void (async () => {
        event.preventDefault()
        sidebar.classList.remove("drag-over")
        this.stopAutosaving()
    
        const files = event.dataTransfer?.files
        if (!files || files.length === 0) {
          return
        }
    
        for (const file of files) {
          try {
            const content = await file.text()
            const filename = file.name
    
            if (await this.fs.fileExists(filename)) {
              const shouldOverwrite = confirm(
                `File "${filename}" already exists. Do you want to overwrite it?`,
              )
              if (!shouldOverwrite) {
                continue
              }
    
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
      })()
    })
  }    

  private initButtons() {
    const toggleSidebar = document.getElementById("toggle-sidebar")
    const sidebarButton = document.getElementById("sidebar")
    
    if (toggleSidebar === null || sidebarButton === null) {
      throw new Error("toggle-sidebar or sidebar not found")
    }
    
    toggleSidebar.addEventListener("click", () => {
      const sidebar = sidebarButton
      const isVisible = sidebar.style.display !== "none"
      sidebar.style.display = isVisible ? "none" : "block"
    })

    ///// File Buttons ////////////////////////////////////////////////////

    const createFile = document.getElementById("create-file")
    if (createFile === null) {
      throw new Error("create-file button not found")
    }
    
    createFile.addEventListener("click", () => {
      void (async () => {
        const filename = prompt("Enter a file name for your new program.")
        if (filename !== null) {
          if (await this.fs.fileExists(filename)) {
            alert(`File ${filename} already exists!`)
          } else {
            await this.fs.saveFile(filename, `; ${filename}`)
            await this.switchToFile(filename)
          }
        }
      })()
    })

    const uploadFile = document.getElementById("upload-file")
    const uploadFileInput = document.getElementById("upload-file-input")
    
    if (uploadFile === null || !(uploadFileInput instanceof HTMLInputElement)) {
      throw new Error("upload-file or upload-file-input not found")
    }
    
    uploadFile.addEventListener("click", () => {
      uploadFileInput.click()
    })

    uploadFileInput.addEventListener("change", (event) => {
      void (async () => {
        const target = event.target as HTMLInputElement
        const file = target.files?.[0]
        if (!file) {
          return
        }

        try {
          const content = await file.text()
          const filename = file.name

          // Check if file already exists
          if (await this.fs.fileExists(filename)) {
            const shouldOverwrite = confirm(
              `File "${filename}" already exists. Do you want to overwrite it?`,
            )
            if (!shouldOverwrite) {
              target.value = "" // Reset the input
              return
            }
            // Delete existing file
            this.stopAutosaving()
            await this.fs.deleteFile(filename)
          }

          await this.fs.saveFile(filename, content)
          this.currentFile = null
          await this.switchToFile(filename)

          // Reset the file input for future uploads
          target.value = ""
        } catch (e) {
          if (e instanceof Error) {
            this.displayError(`Failed to upload file: ${e.message}`)
          }
          target.value = "" // Reset the input on error
        }
      }) ()
    })
    const downloadArchive = document.getElementById("download-archive")
    if (downloadArchive === null) {
      throw new Error("download-archive button not found")
    }
    
    downloadArchive.addEventListener("click", () => {
      // TODO: implement logic for zipping all the files in the
      // file system as a backup
    })

      const renameFile = document.getElementById("rename-file")
      if (renameFile === null) {
        throw new Error("rename-file button not found")
      }
      
      renameFile.addEventListener("click", () => {
        void (async () => {
        if (!this.currentFile) {
          return
        }
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
      }) ()
    })

    const deleteFile = document.getElementById("delete-file")
    if (deleteFile === null) {
      throw new Error("delete-file button not found")
    }
    
    deleteFile.addEventListener("click", () => {
      void (async () => {
        if (!this.currentFile) {
          return
        }
        const shouldDelete = confirm(
          `Are you sure you want to delete ${this.currentFile}?`,
        )
        if (shouldDelete) {
          this.stopAutosaving()
          await this.fs.deleteFile(this.currentFile)

          // Remove the file from output in the IDE
          this.currentFile = null
          this.initializeDummyDoc()
          this.config.lastOpenedFilename = null
          outputPane.innerHTML = ""

          void this.populateFileDrawer()
          this.startAutosaving()
        }
      }) ()
    })

    const downloadFile = document.getElementById("download-file")
    if (downloadFile === null) {
      throw new Error("download-file button not found")
    }
    
    downloadFile.addEventListener("click", () => {
      void (async () => {
        if (!this.currentFile) {
          return
        }
        const contents = await this.fs.loadFile(this.currentFile)
        const hidden = document.createElement("a")
        hidden.href =
          "data:attachment/text;charset=utf-8," + encodeURIComponent(contents)
        hidden.target = "_blank"
        hidden.download = this.currentFile
        hidden.click()
      }) ()
    })

    ///// Execution Buttons ////////////////////////////////////////////////////

    const runBtn = document.getElementById("run")
    if (!runBtn) throw new Error("runBtn or inProgress not found")
    const {
      el: inProgress,
      stopBtn,
      showProgress,
      hideProgress,
    } = mkInProgress("stop")
    runBtn.after(inProgress)

    runBtn.addEventListener("click", () => {
      void (async () => {
      this.startScamper(false)
      runBtn.style.display = "none"
      showProgress()
      try {
        await this.scamper?.runProgram()
      } finally {
        runBtn.style.display = ""
        hideProgress()
      }
    }) ()
    })
    
    stopBtn.addEventListener("click", () => {
      this.scamper?.cancel()
    }) 

    document
    .getElementById("run-window")
    ?.addEventListener("click", () => {
      void (async () => {
        if (!this.currentFile) {
          return
        }
        await this.saveCurrentFile()
        const params = new URLSearchParams({
          filename: this.currentFile,
          isTree: "false",
        })
        window.open(`runner.html?${params.toString()}`)
      })()
    })

      const step = document.getElementById("step")
      if (step === null) {
        throw new Error("step button not found")
      }
      
      step.addEventListener("click", () => {
      this.startScamper(true)
    })

    const stepOnce = document.getElementById("step-once")
    if (stepOnce === null) {
      throw new Error("step-once button not found")
    }
    
    stepOnce.addEventListener("click", () => {
      if (this.scamper === undefined) {
        return
      }
      this.scamper.stepProgram()
      outputPane.scrollTo(0, outputPane.scrollHeight)
    })

    const stepStmtBtn = document.getElementById("step-stmt")
    if (!stepStmtBtn) throw new Error("step-stmt button not found?")
    const {
      el: inProgressTraceStmt,
      stopBtn: stopBtnTraceStmt,
      hideProgress: hideProgressTraceStmt,
      showProgress: showProgressTraceStmt,
    } = mkInProgress("stop-trace-stmt")
    stepStmtBtn.after(inProgressTraceStmt)
    stepStmtBtn.addEventListener("click", () => {
      void (async () => {
      // switch visibility of the step-all button and the trace in progress div
      stepStmtBtn.style.display = "none"
      showProgressTraceStmt()
      try {
        await this.scamper?.stepStmtProgram()
      } finally {
        stepStmtBtn.style.display = ""
        hideProgressTraceStmt()
      }
      outputPane.scrollTo(0, outputPane.scrollHeight)
    }) ()
    })

    stopBtnTraceStmt.addEventListener("click", () => {
      this.scamper?.cancel()
    })

    const stepAllBtn = document.getElementById("step-all")
    if (!stepAllBtn) throw new Error("trace all button not found?")
    const {
      el: inProgressTraceAll,
      stopBtn: stopBtnTraceAll,
      hideProgress: hideProgressTraceAll,
      showProgress: showProgressTraceAll,
    } = mkInProgress("stop-trace-all")
    stepAllBtn.after(inProgressTraceAll)

    stepAllBtn.addEventListener("click", () => {
      void (async () => {
      // switch visibility of the step-all button and the trace in progress div
      stepAllBtn.style.display = "none"
      showProgressTraceAll()
      try {
        // smaller steps per yield due to no virtualized lists
        await this.scamper?.runProgram()
      } finally {
        stepAllBtn.style.display = ""
        hideProgressTraceAll()
      }
      outputPane.scrollTo(0, outputPane.scrollHeight)
    }) ()
    })
    stopBtnTraceAll.addEventListener("click", () => {
      this.scamper?.cancel()
    })

    const astText = document.getElementById("ast-text")
    if (astText === null) {
      throw new Error("ast-text button not found")
    }

astText.addEventListener("click", () => {
      this.showASTText()
    })

    const astWindow = document.getElementById("ast-window")
    if (astWindow === null) {
      throw new Error("ast-window button not found")
    }
    
    astWindow.addEventListener("click", () => {
      if (!this.currentFile) {
        return
      }
      void this.saveCurrentFile()
      const params = new URLSearchParams({
        filename: this.currentFile,
        isTree: "true",
      })
      window.open(`runner.html?${params.toString()}`)
    })

    this.toggleStepButtons(false)
  }

  /** Toggles the step buttons on (true) or off (false). */
  private toggleStepButtons(enabled: boolean) {
    const stepOnce = document.getElementById("step-once")
    const stepStmt = document.getElementById("step-stmt")
    const stepAll = document.getElementById("step-all")
    
    if (
      !(stepOnce instanceof HTMLButtonElement) ||
      !(stepStmt instanceof HTMLButtonElement) ||
      !(stepAll instanceof HTMLButtonElement)
    ) {
      throw new Error("step buttons not found")
    }
    stepOnce.disabled = !enabled
    stepStmt.disabled = !enabled
    stepAll.disabled = !enabled
  }

  /** Populates the file drawer with entries. */
  private async populateFileDrawer() {
    const fileDrawer = document.getElementById("file-drawer")
    if (fileDrawer === null) {
      throw new Error("file-drawer not found")
    }
    // N.B., empty the container and repopulate from scratch
    fileDrawer.innerHTML = ""
    const files = await this.fs.getFileList()
    let tabIndex = 0
    for (const file of files) {
      if (
        !file.isDirectory &&
        (this.showDotFiles || !file.name.startsWith("."))
      ) {
        const ret = document.createElement("div")
        ret.setAttribute("role", "button")
        ret.setAttribute("aria-label", `Open ${file.name}`)
        ret.setAttribute("tabindex", (tabIndex++).toString())
        ret.classList.add("file")
        if (file.name === this.currentFile) {
          ret.classList.add("selected")
        }
        ret.textContent = file.name
        ret.addEventListener("click", () => {
          void (async () => {
          // N.B., try to avoid double-clicking causing multiple file loads to occur at once
          if (!this.isLoadingFile) {
            await this.switchToFile(file.name)
          }
        }) ()
        })
        fileDrawer.appendChild(ret)
      }
    }
  }

  /** Saves the current configuration to disk */
  async saveConfig() {
    await this.fs.saveFile(configFilename, JSON.stringify(this.config))
  }

  /** Loads the configuration from disk */
  async loadConfig() {
    if (await this.fs.fileExists(configFilename)) {
      this.config = JSON.parse(await this.fs.loadFile(configFilename)) as Config
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
      const loadingContent = document.getElementById("loading-content")
      const loading = document.getElementById("loading")
      
      if (loadingContent === null || loading === null) {
        throw new Error("loading elements not found")
      }
      
      loadingContent.innerText =
        "Another instance of Scamper is open. Please close that instance and try again."
      loading.style.display = "block"
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
      this.autosaveId = window.setInterval(() => {
        void this.saveCurrentFile()
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
        output: outputPane,
        dirtyAction: () => {
          this.makeDirty()
        },
        isReadOnly: false,
      }),
    )
  }

  initializeDummyDoc() {
    this.editor.setState(mkNoFileEditorState(outputPane))
  }

  makeDirty() {
    if (this.scamper !== undefined && !this.isDirty) {
      this.isDirty = true
      // const status = document.getElementById('results-status')!
      const msg = document.createElement("em")
      msg.innerText = "(Warning: results out of sync with updated code)"
      const resultsStatus = document.getElementById("results-status")
      if (resultsStatus === null) {
        throw new Error("results-status not found")
      }
      resultsStatus.appendChild(msg)
    }
  }

  makeClean() {
    const resultsStatus = document.getElementById("results-status")
    if (resultsStatus === null) {
      throw new Error("results-status not found")
    }
    resultsStatus.innerHTML = ""
    this.isDirty = false
  }

  ///// IDE actions ////////////////////////////////////////////////////////////

  startScamper(isTracing: boolean): void {
    outputPane.innerHTML = ""
    try {
      this.scamper = new Scamper(outputPane, this.getDoc(), isTracing)
      if (isTracing) {
        this.toggleStepButtons(true)
      } else {
        this.toggleStepButtons(false)
      }
    } catch (e) {
      renderToOutput(outputPane, e)
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
      // const parsed = Parser.parseProgram(this.getDoc())
      // const labelEl = document.createElement('h2')
      // labelEl.setAttribute('id', 'ast-label')
      // labelEl.innerText = "Abstract Syntax Tree"
      // labelEl.setAttribute('tabindex', '0')
      // labelEl.setAttribute('aria-label', 'Abstract Syntax Tree... Navigation instructions: use tab to traverse the tree in the order of node position on the code, or use "left/right" arrows for visiting neighbors, "down arrow" to visit children, and "up arrow" to go to parent')
      // outputPane!.appendChild(labelEl)
      // parsed.ast.render(outputPane, this.editor)
      // const descriptionEl = document.createElement('div')
      // descriptionEl.setAttribute('id', 'ast-desc')
      // descriptionEl.innerText = parsed.ast.describe()
      // descriptionEl.setAttribute('tabindex', '0')
      // descriptionEl.setAttribute('role', 'region')
      // outputPane!.appendChild(descriptionEl)
      // this.makeClean()
    } catch (e) {
      renderToOutput(outputPane, e)
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
        this.displayError(`${e.message}\n\n${String(e.stack)}`)
      }
    }

    // Reset the UI: output panel and file drawer, also restart saving
    outputPane.innerHTML = ""
    void this.populateFileDrawer()
    this.config.lastOpenedFilename = this.currentFile
    this.startAutosaving()
    this.isLoadingFile = false
  }

  displayError(error: string) {
    const loadingContent = document.getElementById("loading-content")
    const loading = document.getElementById("loading")
    
    if (loadingContent === null || loading === null) {
      throw new Error("loading elements not found")
    }
    
    loadingContent.innerText = error
    loading.style.display = "block"
  }
}

const idePromise = IDE.create()
void idePromise
export const ide = idePromise
