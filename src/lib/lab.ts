import * as R from '../lpm/runtime.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'

const Lab: R.Library = new R.Library()

function title (text: string) {
  checkContract(arguments, contract('title', [C.string]))
  const ret = document.createElement('h1')
  ret.innerText = text
  return ret
}
Lab.registerValue('title', title)

function part (text: string) {
  checkContract(arguments, contract('part', [C.string]))
  const ret = document.createElement('h2')
  ret.innerText = text
  return ret
}
Lab.registerValue('part', part)

function problem (text: string) {
  checkContract(arguments, contract('problem', [C.string]))
  const ret = document.createElement('h3')
  ret.innerText = text
  return ret
}
Lab.registerValue('problem', problem)

function description (text: string) {
  checkContract(arguments, contract('description', [C.string]))
  const ret = document.createElement('p')
  const em = document.createElement('em')
  em.innerText = text
  ret.appendChild(em)
  return ret
}
Lab.registerValue('description', description)

export default Lab