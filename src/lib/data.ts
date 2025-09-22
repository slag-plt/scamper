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
  const words = s.split(/\s+/g) || []
  return L.vectorToList(words)
}
Data.registerValue('string->words', stringToWords)

function stringToLines (s: string): L.List {
  checkContract(arguments, contract('string->lines', [C.string]))
  const lines = s.split(/\r?\n/g) || []
  return L.vectorToList(lines)
}
Data.registerValue('string->lines', stringToLines)

export default Data