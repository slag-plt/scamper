import { describe, expect, test } from 'vitest'
import {
  color_colorNameToRgb,
  color_isColorName,
} from '../../src/js/image/color'

// Found while removing the non-null assertions (#154). `color_isColorName`
// lower-cases the name it checks and the lookup beside it did not, so
// `(color-name->rgb "RED")` passed the guard, missed the table, and handed back
// an `undefined` that the assertion had typed as an Rgb. Nothing complained
// until the value was used.
describe('a colour name is recognised whatever its case', () => {
  test.each(['RED', 'Red', 'rEd', 'red'])('%s resolves to red', (name) => {
    expect(color_isColorName(name)).toBe(true)
    const rgb = color_colorNameToRgb(name)
    expect(rgb).toBeDefined()
    expect(rgb.red).toBe(255)
    expect(rgb.green).toBe(0)
    expect(rgb.blue).toBe(0)
  })

  test('a name that is not a colour still fails loudly', () => {
    expect(color_isColorName('mauvish')).toBe(false)
    expect(() => color_colorNameToRgb('mauvish')).toThrow(/unknown color name/)
  })
})
