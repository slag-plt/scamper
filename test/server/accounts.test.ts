import { describe, expect, test } from 'vitest'
import { generatePassword } from '../../server/src/accounts'

describe('generated passwords', () => {
  // These are read aloud, written on paper, and typed by someone who did not
  // choose them, because there is no mail server to send them through.
  const AMBIGUOUS = ['0', 'O', '1', 'l', 'I', '5', 'S', '8', 'B']

  test('is long enough to be worth generating', () => {
    expect(generatePassword()).toHaveLength(16)
  })

  test('avoids characters that are misread when transcribed', () => {
    // Every character of a hundred passwords, so a rare draw is still caught.
    const drawn = new Set(
      Array.from({ length: 100 }, () => generatePassword()).join(''),
    )
    for (const character of AMBIGUOUS) {
      expect([...drawn]).not.toContain(character)
    }
  })

  test('does not repeat itself', () => {
    const passwords = new Set(
      Array.from({ length: 100 }, () => generatePassword()),
    )
    expect(passwords.size).toBe(100)
  })
})
