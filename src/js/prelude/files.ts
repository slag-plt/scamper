import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'

///// Reactive file ////////////////////////////////////////////////////////////

export interface ReactiveFile extends L.Struct {
  [L.structKind]: 'reactive-file',
  filename: string,
  callback: L.ScamperFn
}

export function withFile(filename: string, callback: L.ScamperFn): ReactiveFile {
  checkContract(arguments, contract('with-file', [C.string, C.func]))
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

export function withFileChooser(callback: L.ScamperFn): ReactiveFileChooser {
  checkContract(arguments, contract('with-file-chooser', [C.func]))
  return {
    [L.scamperTag]: 'struct',
    [L.structKind]: 'reactive-file-chooser',
    callback
  }
}