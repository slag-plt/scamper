import * as L from '../lpm'
import { checkContract, contract } from './contract.js'
import * as C from './contract.js'

import Papa from 'papaparse'

const Data: L.Library = new L.Library()

function parseCsv (data: string): L.List {
  checkContract(arguments, contract('parse-csv', [C.string]))
  const result = Papa.parse(data, { header: false })
  if (result.errors.length > 0) {
    const errStr = result.errors.map(e => {
      return `${e.type} (row ${e.row}): ${e.message}`
    }).join('\n')
    throw new L.ScamperError('Runtime', `Error(s) parsing CSV files:\n${errStr}`)
  } else {
    return L.vectorToList((result.data as string[][]).map(L.vectorToList))
  }
}
Data.registerValue('parse-csv', parseCsv)

function stringToChars (s: string): L.List {
  checkContract(arguments, contract('string->chars', [C.string]))
  const chars = Array.from(s).map(c => L.mkChar(c))
  return L.vectorToList(chars)
}
Data.registerValue('string->chars', stringToChars)

function stringToWords (s: string): L.List {
  checkContract(arguments, contract('string->words', [C.string]))
  const words = s.split(/\s+/g)
  return L.vectorToList(words)
}
Data.registerValue('string->words', stringToWords)

function stringToLines (s: string): L.List {
  checkContract(arguments, contract('string->lines', [C.string]))
  const lines = s.split(/\r?\n/g)
  return L.vectorToList(lines)
}
Data.registerValue('string->lines', stringToLines)

/**
 * A simple association list over LPM values for the purposes of tallying. We
 * reimplement the basic functionality of a dictionary because the JS Map
 * datatype does not allow for arbitrary equality over keys.
 */
class TallyList {
  public data: { key: L.Value, value: number }[]

  constructor () {
    this.data = []
  }

  update (key: L.Value, updater: (value: number) => number, def: number): void {
    for (let i = 0; i < this.data.length; i++) {
      if (L.equals(this.data[i].key, key)) {
        this.data[i].value = updater(this.data[i].value)
        return
      }
    }
    this.data.push({ key, value: def })
  }
}

function tallyAll (lst: L.List): L.List {
  const tally = new TallyList()
  let cur: L.List = lst
  while (cur !== null) {
    const value = cur.head
    tally.update(value, v => v + 1, 1)
    cur = cur.tail
  }
  return L.vectorToList(tally.data.map(p => L.mkPair(p.key, p.value)))
}
Data.registerValue('tally-all', tallyAll)

export default Data