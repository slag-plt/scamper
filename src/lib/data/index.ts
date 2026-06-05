import Utils from './utils.js'
import Viz from './viz.js'
import * as L from '../../lpm'

const Data: L.Module = L.Module.fromModules(Utils, Viz)

export default Data