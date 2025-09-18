import * as L from './lang'

export interface TraceOutput extends L.Struct {
  [L.structKind]: 'trace-output'
  output: L.Value
}