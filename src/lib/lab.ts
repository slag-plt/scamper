import { Value } from '../lang.js'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'

function registerFn (name: string, fn: Function, map: [string, Value.T][]) {
  Value.nameFn(name, fn)
  map.push([name, fn])
}

const Lab: [string, Value.T][] = []

function title (text: string) {
  checkContract(arguments, contract('title', [C.string]))
  const ret = document.createElement('h1')
  ret.innerText = text
  return ret
}
registerFn('title', title, Lab)

function part (text: string) {
  checkContract(arguments, contract('part', [C.string]))
  const ret = document.createElement('h2')
  ret.innerText = text
  return ret
}
registerFn('part', part, Lab)

function problem (text: string) {
  checkContract(arguments, contract('problem', [C.string]))
  const ret = document.createElement('h3')
  ret.innerText = text
  return ret
}
registerFn('problem', problem, Lab)

function description (text: string) {
  checkContract(arguments, contract('description', [C.string]))
  const ret = document.createElement('p')
  const em = document.createElement('em')
  em.innerText = text
  ret.appendChild(em)
  return ret
}
registerFn('description', description, Lab)

export default Lab