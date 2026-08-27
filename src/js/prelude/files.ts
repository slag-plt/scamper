import * as L from '../../lpm'

///// Blocking file read ///////////////////////////////////////////////////////

// N.B., suspends the current fiber, asks the scheduler to load `filename`
// asynchronously, and resumes with its contents as this call's return value
// (see SuspendSignal / Scheduler `block-on` handling). A missing file rejects,
// surfacing as a runtime error catchable by with-handler. Used by the Scheme
// `with-file` wrapper -- a JS function can no longer call the user's callback.
//
// The filesystem is imported lazily inside the action, NOT at module load: this
// module is pulled in during test setup (prelude registration), and eagerly
// importing src/fs there would grab the real OPFS out from under tests that mock
// it (see test/setup.ts).
export function prelude_blockOnReadFile(filename: string): L.Value {
  throw new L.SuspendSignal(async () => {
    const { getFS } = await import('../../fs')
    const fs = getFS()
    if (!(await fs.fileExists(filename))) {
      throw new L.ScamperError('Runtime', `File "${filename}" does not exist`)
    }
    return fs.loadFile(filename)
  })
}

///// Reactive file chooser ////////////////////////////////////////////////////

export interface ReactiveFileChooser extends L.Struct {
  [L.structKind]: 'reactive-file-chooser',
  callback: L.ScamperFn,
  [L.runField]: L.RunHandle
}

export function prelude_withFileChooser(callback: L.ScamperFn): ReactiveFileChooser {
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file-chooser',
    callback,
    // Captured here, where it can be: this runs during a step, whereas Vue
    // mounts the renderer afterwards, when there is no current run left to
    // resolve and the free `spawn` falls back to the foreground one. In a
    // reading there is no foreground run, so the callback was dropped (#397).
    [L.runField]: L.currentRun()
  }
}