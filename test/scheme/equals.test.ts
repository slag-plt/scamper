import { expect, describe, test } from 'vitest'
import * as L from '../../src/lpm'
import * as S from '../../src/scheme'

test('simple program equality', () => {
  const err = new L.LoggingChannel()
  // expect(S.raiser.equals(
  //   S.compile(err, `
  //     "Hello world!"
  //   `),
  //   S.compile(err, `
  //     "Hello world!"
  //   `))).toBe(true)
})