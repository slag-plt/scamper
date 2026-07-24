import { describe, expect, test } from 'vitest'
import { runProgram } from '../harness.js'
import { lab_title, lab_part, lab_problem, lab_description } from '../../src/js/lab/index.js'

// N.B., these functions set content via `.innerText`, which jsdom stores
// but never reflects into `.textContent`/`.innerHTML` (jsdom doesn't
// implement layout), so we check `.innerText` here rather than
// `.textContent`.

describe('html?', () => {
  test('is true for an element made by title/part/problem/description', async () => {
    expect(await runProgram(`
    (import lab)
    (html? (title "t"))
    (html? (part "p"))
    (html? (problem "pr"))
    (html? (description "d"))
    `)).toEqual(['#t', '#t', '#t', '#t'])
  })

  test('is false for non-element values', async () => {
    expect(await runProgram(`
    (import lab)
    (html? 5)
    (html? "html")
    (html? #t)
    (html? (list 1 2 3))
    (html? (pair 1 2))
    `)).toEqual(['#f', '#f', '#f', '#f', '#f'])
  })
})

describe('title', () => {
  test('creates an h1 with the given text', () => {
    const elt = lab_title('My Lab')
    expect(elt.tagName).toBe('H1')
    expect(elt.innerText).toBe('My Lab')
  })

  test('an empty string produces an empty h1', () => {
    const elt = lab_title('')
    expect(elt.tagName).toBe('H1')
    expect(elt.innerText).toBe('')
  })

  // N.B., the reported range points at title's own definition in lab.scm
  // rather than the call site -- a known, unrelated limitation of
  // contract-wrapped errors (see prelude.test.ts's not-boolean test).
  test('a non-string argument fails the contract check', async () => {
    expect(await runProgram(`
    (import lab)
    (title 5)
    `)).toEqual([
      'Runtime error [10:1-10:35]: (error) expected a string, received number',
    ])
  })
})

describe('part', () => {
  test('creates an h2 with the given text', () => {
    const elt = lab_part('Part 1')
    expect(elt.tagName).toBe('H2')
    expect(elt.innerText).toBe('Part 1')
  })

  test('a non-string argument fails the contract check', async () => {
    expect(await runProgram(`
    (import lab)
    (part 5)
    `)).toEqual([
      'Runtime error [16:1-16:33]: (error) expected a string, received number',
    ])
  })
})

describe('problem', () => {
  test('creates an h3 with the given text', () => {
    const elt = lab_problem('Problem 1')
    expect(elt.tagName).toBe('H3')
    expect(elt.innerText).toBe('Problem 1')
  })

  test('a non-string argument fails the contract check', async () => {
    expect(await runProgram(`
    (import lab)
    (problem 5)
    `)).toEqual([
      'Runtime error [22:1-22:39]: (error) expected a string, received number',
    ])
  })
})

describe('description', () => {
  test('creates a p containing an em with the given text', () => {
    const elt = lab_description('Some description')
    expect(elt.tagName).toBe('P')
    const em = elt.querySelector('em')
    expect(em).not.toBeNull()
    expect(em?.innerText).toBe('Some description')
  })

  test('a non-string argument fails the contract check', async () => {
    expect(await runProgram(`
    (import lab)
    (description 5)
    `)).toEqual([
      'Runtime error [28:1-28:47]: (error) expected a string, received number',
    ])
  })
})
