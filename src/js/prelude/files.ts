import * as L from '../../lpm'

///// Reactive file ////////////////////////////////////////////////////////////

export interface ReactiveFile extends L.Struct {
  [L.structKind]: 'reactive-file',
  filename: string,
  callback: L.ScamperFn
}

export function prelude_withFile(filename: string, callback: L.ScamperFn): ReactiveFile {
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file',
    filename,
    callback
  }
}

///// Reactive file chooser ////////////////////////////////////////////////////

export interface ReactiveFileChooser extends L.Struct {
  [L.structKind]: 'reactive-file-chooser',
  callback: L.ScamperFn
}

export function prelude_withFileChooser(callback: L.ScamperFn): ReactiveFileChooser {
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file-chooser',
    callback
  }
}