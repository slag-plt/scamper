import { emptyLibrary, Library, registerValue } from '../lang.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'

const Lab: Library = emptyLibrary()

function title (text: string) {
  checkContract(arguments, contract('title', [C.string]))
  const ret = document.createElement('h1')
  ret.innerText = text
  return ret
}
registerValue('title', title, Lab)

function part (text: string) {
  checkContract(arguments, contract('part', [C.string]))
  const ret = document.createElement('h2')
  ret.innerText = text
  return ret
}
registerValue('part', part, Lab)

function problem (text: string) {
  checkContract(arguments, contract('problem', [C.string]))
  const ret = document.createElement('h3')
  ret.innerText = text
  return ret
}
registerValue('problem', problem, Lab)

function description (text: string) {
  checkContract(arguments, contract('description', [C.string]))
  const ret = document.createElement('p')
  const em = document.createElement('em')
  em.innerText = text
  ret.appendChild(em)
  return ret
}
registerValue('description', description, Lab)

export default Lab