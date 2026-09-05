import { requireBrowser } from '../browser.js'

export function lab_title(text: string) {
  requireBrowser()
  const ret = document.createElement('h1')
  ret.innerText = text
  return ret
}

export function lab_part(text: string) {
  requireBrowser()
  const ret = document.createElement('h2')
  ret.innerText = text
  return ret
}

export function lab_problem(text: string) {
  requireBrowser()
  const ret = document.createElement('h3')
  ret.innerText = text
  return ret
}

export function lab_description(text: string) {
  requireBrowser()
  const ret = document.createElement('p')
  const em = document.createElement('em')
  em.innerText = text
  ret.appendChild(em)
  return ret
}
