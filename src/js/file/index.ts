import * as L from '../../lpm'

// The `file` library's blocking primitives (issue #315). Each suspends the
// current fiber, hands the scheduler an async filesystem action, and resumes
// with its result as the call's return value (see SuspendSignal / Scheduler
// `block-on`). A rejected action surfaces as a runtime error at the call site,
// catchable by with-handler.
//
// N.B., the filesystem is imported lazily *inside* each action, never at module
// load: this module is pulled in during test setup (library registration), and
// eagerly importing src/fs there would grab the real OPFS out from under tests
// that mock it. Same reasoning as src/js/prelude/files.ts.

/** Loads `filename`, or rejects if it does not exist. */
async function readFile(filename: string): Promise<string> {
  const { getFS } = await import('../../fs')
  const fs = getFS()
  if (!(await fs.fileExists(filename))) {
    throw new L.ScamperError('Runtime', `File "${filename}" does not exist`)
  }
  return fs.loadFile(filename)
}

/** Writes `contents` to `filename`, creating or overwriting it. */
async function writeFile(filename: string, contents: string): Promise<void> {
  const { getFS } = await import('../../fs')
  await getFS().saveFile(filename, contents)
}

/**
 * Splits file contents into lines, dropping the single empty element a trailing
 * newline would otherwise produce -- a file ending in "\n" has no final empty
 * line. This is what makes lines->file and file->lines round-trip.
 *
 * N.B., deliberately not data_stringToLines: that one keeps the trailing empty
 * element (its callers want the raw split), and reusing it would make the
 * `file` library depend on `data`.
 */
function splitLines(contents: string): string[] {
  const lines = contents.split(/\r?\n/g)
  if (lines.length > 0 && lines[lines.length - 1] === '') {
    lines.pop()
  }
  return lines
}

export function file_fileToString(filename: string): L.Value {
  throw new L.SuspendSignal(async () => readFile(filename))
}

export function file_fileToLines(filename: string): L.Value {
  throw new L.SuspendSignal(async () =>
    L.vectorToList(splitLines(await readFile(filename))),
  )
}

export function file_stringToFile(contents: string, filename: string): L.Value {
  throw new L.SuspendSignal(async () => {
    await writeFile(filename, contents)
    return undefined
  })
}

export function file_linesToFile(lines: L.List, filename: string): L.Value {
  throw new L.SuspendSignal(async () => {
    // Checked here rather than by a docstring contract: `list?` cannot say
    // "list *of strings*", and a non-string element would otherwise be
    // stringified into the file by join(). Collecting into a string[] (rather
    // than checking in place) is what lets the join below be type-safe.
    const strs: string[] = L.listToVector(lines).map((v) => {
      if (typeof v !== 'string') {
        throw new L.ScamperError(
          'Runtime',
          `lines->file: expected a list of strings, but the list contains ${L.typeOf(v)}`,
        )
      }
      return v
    })
    // Joined with "\n" plus a trailing newline, so the file ends in a newline
    // and file->lines recovers exactly `lines`. An empty list writes an empty
    // file rather than a lone newline.
    await writeFile(filename, strs.length === 0 ? '' : `${strs.join('\n')}\n`)
    return undefined
  })
}
