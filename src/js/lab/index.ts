import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'

export function lab_title(text: string) {
  checkContract(arguments, contract('title', [C.string]))
  const ret = document.createElement('h1')
  ret.innerText = text
  return ret
}

export function lab_part(text: string) {
  checkContract(arguments, contract('part', [C.string]))
  const ret = document.createElement('h2')
  ret.innerText = text
  return ret
}

export function lab_problem(text: string) {
  checkContract(arguments, contract('problem', [C.string]))
  const ret = document.createElement('h3')
  ret.innerText = text
  return ret
}

export function lab_description(text: string) {
  checkContract(arguments, contract('description', [C.string]))
  const ret = document.createElement('p')
  const em = document.createElement('em')
  em.innerText = text
  ret.appendChild(em)
  return ret
}
