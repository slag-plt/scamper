import { describe, expect, test } from 'vitest'
import * as L from '../../src/lpm'
import { runProgram } from '../harness.js'
import { image_isReactiveImageFile, image_withImageFile } from '../../src/js/image/image.js'
import { image_imageWidth, image_imageHeight } from '../../src/js/image/drawing.js'

describe('color', () => {
  test('color', async () => {
    expect(
      await runProgram(`
(import image)
(color 255 0 0 255)
(color 0 0 0 0)
(color 300 0 0 255)
(color -10 0 0 255)
(color 255 0 0)
(color 255.5 0 0 255)
`),
    ).toEqual([
      '(rgba 255 0 0 255)',
      '(rgba 0 0 0 0)',
      // r is only ever clamped from above (Math.min(r, 255)) -- out-of-range
      // integers above 255 get silently clamped rather than rejected, since
      // color's own docstring contract only checks integer?, not the
      // 0-255 range its description mentions.
      '(rgba 255 0 0 255)',
      // ...and never clamped from below, so a negative integer passes through as-is.
      '(rgba -10 0 0 255)',
      'Runtime error [6:1-6:15]: Arity mismatch in function call: expected 4 arguments, got 3',
      'Runtime error [22:1-22:37]: (error) expected an integer, received number',
    ])
  })

  test('color?', async () => {
    expect(
      await runProgram(`
(import image)
(color? "red")
(color? (rgb 255 0 0))
(color? (hsv 0 0 0))
(color? "not-a-color-name")
(color? 5)
(color? #t)
`),
    ).toEqual(['#t', '#t', '#t', '#f', '#f', '#f'])
  })

  test('rgb-component?', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-component? 0)
(rgb-component? 255)
(rgb-component? 128)
(rgb-component? -1)
(rgb-component? 256)
(rgb-component? 255.5)
(rgb-component? "abc")
`),
    ).toEqual(['#t', '#t', '#t', '#f', '#f', '#f', '#f'])
  })

  test('rgb?', async () => {
    expect(
      await runProgram(`
(import image)
(rgb? (rgb 1 2 3))
(rgb? (hsv 1 2 3))
(rgb? "red")
(rgb? 5)
(rgb? '())
`),
    ).toEqual(['#t', '#f', '#f', '#f', '#f'])
  })

  test('rgb', async () => {
    expect(
      await runProgram(`
(import image)
(rgb 255 0 0)
(rgb 0 0 0 0)
(rgb 1 2 3 4 5)
`),
    ).toEqual([
      '(rgba 255 0 0 255)',
      '(rgba 0 0 0 0)',
      'Runtime error [4:1-4:15]: (rgb) rgb: expects 3 or 4 arguments, but got 5',
    ])
  })

  test('rgb failures', async () => {
    expect(
      await runProgram(`
(import image)
(rgb 1 2)
(rgb "x" 0 0)
(rgb 1 2 3 300)
`),
    ).toEqual([
      'Runtime error [2:1-2:9]: Arity mismatch in function call: expected 3 arguments, got 2',
      'Runtime error [50:1-50:33]: (error) expected a rgb-component, received string',
      'Runtime error [50:1-50:33]: (error) expected every value of a to be a rgb-component, but at least one was not',
    ])
  })

  test('rgb-red', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-red (rgb 10 20 30))
(rgb-red 5)
(rgb-red)
(rgb-red 1 2)
`),
    ).toEqual([
      '10',
      'Runtime error [56:1-56:40]: (error) expected a rgb, received number',
      'Runtime error [4:1-4:9]: Arity mismatch in function call: expected 1 arguments, got 0',
      'Runtime error [5:1-5:13]: Arity mismatch in function call: expected 1 arguments, got 2',
    ])
  })

  test('rgb-green', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-green (rgb 10 20 30))
(rgb-green "x")
`),
    ).toEqual([
      '20',
      'Runtime error [62:1-62:44]: (error) expected a rgb, received string',
    ])
  })

  test('rgb-blue', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-blue (rgb 10 20 30))
(rgb-blue 'x)
`),
    ).toEqual([
      '30',
      'Runtime error [68:1-68:42]: (error) expected a rgb, received symbol',
    ])
  })

  test('rgb-alpha', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-alpha (rgb 10 20 30 128))
(rgb-alpha (rgb 10 20 30))
(rgb-alpha '())
`),
    ).toEqual([
      '128',
      '255',
      'Runtime error [74:1-74:44]: (error) expected a rgb, received null',
    ])
  })

  test('rgb-distance', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-distance (rgb 0 0 0) (rgb 255 255 255))
(rgb-distance (rgb 10 20 30) (rgb 10 20 30))
(rgb-distance (rgb 0 0 0) '())
`),
    ).toEqual([
      '441.6729559300637',
      '0',
      'Runtime error [81:1-81:50]: (error) expected a rgb, received null',
    ])
  })

  // color-name?, find-colors, and color-name->rgb all write their string
  // parameter's contract as `string` instead of `string?` in image.scm's
  // docstring. The generated contract check calls the real `string`
  // (char ... -> string) constructor as a predicate; since a genuine string
  // argument is never a char, that check's own rest-parameter contract
  // fails for every call, regardless of input, so none of these three can
  // succeed right now.
  describe('blocked by a docstring contract typo (string instead of string?)', () => {
    test.skip('color-name?')
    test.skip('find-colors')
    test.skip('color-name->rgb')
  })

  test('all-color-names', async () => {
    expect(
      await runProgram(`
(import image)
(length (all-color-names 1))
(list-ref (all-color-names 1) 0)
(all-color-names)
`),
    ).toEqual([
      '141',
      '"aliceblue"',
      // the docstring documents a parameter ("x1 : any") that the real
      // implementation (image_allColorNames, which takes none) never uses,
      // so calling with the documented arity actually works, and the
      // "expected" zero-argument call is itself the arity failure.
      'Runtime error [4:1-4:17]: Arity mismatch in function call: expected 1 arguments, got 0',
    ])
  })

  test('rgb->string', async () => {
    expect(
      await runProgram(`
(import image)
(rgb->string (rgb 255 0 0))
(rgb->string (rgb 255 0 0 128))
(rgb->string (rgb 0 0 0 0))
(rgb->string 5)
`),
    ).toEqual([
      '"rgb(255  0  0 / 100%)"',
      '"rgb(255  0  0 / 50%)"',
      '"rgb(0  0  0 / 0%)"',
      'Runtime error [105:1-105:49]: (error) expected a rgb, received number',
    ])
  })

  // hsv? is bound to the hsv constructor (image_hsv) instead of the
  // predicate (image_isHsv) -- calling it tries to construct a struct
  // instead of testing one (github.com/slag-plt/scamper#250).
  test.skip('hsv?')

  test('hsv', async () => {
    expect(
      await runProgram(`
(import image)
(hsv 10 20 30)
(hsv 0 0 0)
(hsv 360 100 100)
(hsv 360 100 100 0)
`),
    ).toEqual([
      '(hsv 10 20 30 255)',
      '(hsv 0 0 0 255)',
      '(hsv 360 100 100 255)',
      '(hsv 360 100 100 0)',
    ])
  })

  test('hsv failures', async () => {
    expect(
      await runProgram(`
(import image)
(hsv 1 2)
(hsv 'a 0 0)
(hsv -1 0 0)
(hsv 361 0 0)
(hsv 0 101 0)
(hsv 0 0 101)
(hsv 0 0 0 256)
`),
    ).toEqual([
      'Runtime error [2:1-2:9]: Arity mismatch in function call: expected 3 arguments, got 2',
      'Runtime error [124:1-124:33]: (error) expected a number, received symbol',
      'Runtime error [4:1-4:12]: (hsv) hsv: expects hue to be in the an angle (0–360), but got -1',
      'Runtime error [5:1-5:13]: (hsv) hsv: expects hue to be in the an angle (0–360), but got 361',
      'Runtime error [6:1-6:13]: (hsv) hsv: expects saturation to be a percentage (0–100), but got 101',
      'Runtime error [7:1-7:13]: (hsv) hsv: expects value to be a percentage (0–100), but got 101',
      'Runtime error [8:1-8:15]: (hsv) hsv: expects alpha to be in the range 0–255, but got 256',
    ])
  })

  test('rgb-hue', async () => {
    // rgb-hue's own implementation picks a random hue when a color is
    // achromatic (max === min channel), so only chromatic colors give a
    // deterministic answer here.
    expect(
      await runProgram(`
(import image)
(rgb-hue (rgb 255 0 0))
(rgb-hue (rgb 0 255 0))
(rgb-hue (rgb 0 0 255))
(rgb-hue (rgb 255 255 0))
(rgb-hue "x")
`),
    ).toEqual([
      '0',
      '120',
      '240',
      '60',
      'Runtime error [160:1-160:40]: (error) expected a rgb, received string',
    ])
  })

  test('rgb-saturation', async () => {
    // image_rgbSaturation passes (max, min) positionally into a helper
    // whose parameters are named (min, max), so the helper's own "max === 0"
    // early-out actually fires whenever the color's minimum channel is 0 --
    // i.e. for most fully-saturated colors. This is real, current (buggy)
    // behavior, not a formula error in this test -- see #259.
    expect(
      await runProgram(`
(import image)
(rgb-saturation (rgb 255 0 0))
(rgb-saturation (rgb 100 100 100))
(rgb-saturation (rgb 200 100 50))
`),
    ).toEqual(['0', '0', '-300'])
  })

  test('rgb-value', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-value (rgb 255 0 0))
(rgb-value (rgb 0 0 0))
(rgb-value (rgb 128 10 10))
`),
    ).toEqual(['100', '0', '50'])
  })

  test('rgb->hsv', async () => {
    expect(
      await runProgram(`
(import image)
(rgb->hsv (rgb 255 0 0))
(rgb->hsv (rgb 0 255 0))
(rgb->hsv (rgb 128 128 128))
(rgb->hsv 5)
`),
    ).toEqual([
      '(hsv 0 100 100 255)',
      '(hsv 120 100 100 255)',
      '(hsv 0 0 50 255)',
      'Runtime error [178:1-178:43]: (error) expected a rgb, received number',
    ])
  })

  // Every function below takes an hsv? argument, and its own contract check
  // calls the buggy hsv? binding (github.com/slag-plt/scamper#250, see the
  // hsv? skip above) before running -- so the check itself throws "hsv:
  // expects 3 or 4 arguments, but got 1" no matter what's passed in, and
  // none of these can succeed right now.
  describe('blocked by #250 (hsv? bound to the hsv constructor)', () => {
    test.skip('hsv-hue')
    test.skip('hsv-saturation')
    test.skip('hsv-value')
    test.skip('hsv-alpha')
    test.skip('hsv-complement')
    test.skip('hsv->string')
    test.skip('hsv->rgb')
  })

  test('rgb-darker', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-darker (rgb 100 100 100))
(rgb-darker (rgb 0 0 0))
(rgb-darker "x")
`),
    ).toEqual([
      '(rgba 84 84 84 255)',
      '(rgba 0 0 0 255)',
      'Runtime error [202:1-202:46]: (error) expected a rgb, received string',
    ])
  })

  test('rgb-lighter', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-lighter (rgb 100 100 100))
(rgb-lighter (rgb 255 255 255))
`),
    ).toEqual(['(rgba 116 116 116 255)', '(rgba 255 255 255 255)'])
  })

  test('rgb-redder', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-redder (rgb 100 100 100))
(rgb-redder (rgb 0 0 0))
`),
    ).toEqual(['(rgba 132 84 84 255)', '(rgba 32 0 0 255)'])
  })

  test('rgb-bluer', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-bluer (rgb 100 100 100))
(rgb-bluer (rgb 0 0 0))
`),
    ).toEqual(['(rgba 84 84 132 255)', '(rgba 0 0 32 255)'])
  })

  test('rgb-greener', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-greener (rgb 100 100 100))
(rgb-greener (rgb 0 0 0))
`),
    ).toEqual(['(rgba 84 132 84 255)', '(rgba 0 32 0 255)'])
  })

  test('rgb-pseudo-complement', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-pseudo-complement (rgb 0 0 0))
(rgb-pseudo-complement (rgb 255 255 255))
(rgb-pseudo-complement "x")
`),
    ).toEqual([
      '(rgba 255 255 255 255)',
      '(rgba 0 0 0 255)',
      'Runtime error [232:1-232:67]: (error) expected a rgb, received string',
    ])
  })

  test('rgb-greyscale', async () => {
    // BUG (#259): image_rgbGreyscale divides the luma sum by 3, but the Rec.601
    // weights already sum to 1.0, so results come out ~3x too dark (white ->
    // 85 instead of 255). These assertions pin the current buggy output.
    expect(
      await runProgram(`
(import image)
(rgb-greyscale (rgb 255 255 255))
(rgb-greyscale (rgb 0 0 0))
(rgb-greyscale (rgb 255 0 0))
`),
    ).toEqual(['(rgba 85 85 85 255)', '(rgba 0 0 0 255)', '(rgba 25.5 25.5 25.5 255)'])
  })

  test('rgb-phaseshift', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-phaseshift (rgb 0 0 0))
(rgb-phaseshift (rgb 200 200 200))
`),
    ).toEqual(['(rgba 128 128 128 255)', '(rgba 72 72 72 255)'])
  })

  test('rgb-rotate-components', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-rotate-components (rgb 1 2 3))
`),
    ).toEqual(['(rgba 2 3 1 255)'])
  })

  test('rgb-thin', async () => {
    // BUG (#259): Math.min(0, alpha - 32) is at most 0 regardless of alpha
    // (should be Math.max(0, ...)), so rgb-thin always drives alpha to 0 or
    // below -- real, current buggy behavior, pinned here.
    expect(
      await runProgram(`
(import image)
(rgb-thin (rgb 1 2 3 200))
(rgb-thin (rgb 1 2 3 0))
`),
    ).toEqual(['(rgba 1 2 3 0)', '(rgba 1 2 3 -32)'])
  })

  test('rgb-thicken', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-thicken (rgb 1 2 3 200))
(rgb-thicken (rgb 1 2 3 255))
`),
    ).toEqual(['(rgba 1 2 3 232)', '(rgba 1 2 3 255)'])
  })

  test('rgb-add', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-add (rgb 1 2 3) (rgb 4 5 6))
(rgb-add (rgb 250 250 250) (rgb 10 10 10))
(rgb-add (rgb 1 2 3))
`),
    ).toEqual([
      '(rgba 5 7 9 255)',
      '(rgba 255 255 255 255)',
      'Runtime error [4:1-4:21]: Arity mismatch in function call: expected 2 arguments, got 1',
    ])
  })

  test('rgb-subtract', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-subtract (rgb 4 5 6) (rgb 1 2 3))
(rgb-subtract (rgb 5 5 5) (rgb 10 10 10))
`),
    ).toEqual(['(rgba 3 3 3 255)', '(rgba 0 0 0 255)'])
  })

  test('rgb-average', async () => {
    expect(
      await runProgram(`
(import image)
(rgb-average (rgb 1 2 3) (rgb 4 5 6))
(rgb-average (rgb 0 0 0) (rgb 0 0 0))
`),
    ).toEqual(['(rgba 2.5 3.5 4.5 255)', '(rgba 0 0 0 255)'])
  })
})

describe('font', () => {
  describe('font?', () => {
    test('is true for a value made by font', async () => {
      expect(
        await runProgram(`
(import image)
(font? (font "Arial" "sans-serif" #f #f))
`),
      ).toEqual(['#t'])
    })

    test('is false for non-font values', async () => {
      expect(
        await runProgram(`
(import image)
(font? 5)
(font? "Arial")
(font? (rectangle 10 10 "solid" "red"))
`),
      ).toEqual(['#f', '#f', '#f'])
    })
  })

  describe('font', () => {
    test('constructs a font struct from all four arguments', async () => {
      expect(
        await runProgram(`
(import image)
(font "Arial" "serif" #t #t)
(font "Georgia" "sans-serif" #f #f)
`),
      ).toEqual([
        '(font "Arial" "serif" #t #t)',
        '(font "Georgia" "sans-serif" #f #f)',
      ])
    })

    test('requires exactly four arguments, despite the docstring calling the last three optional', async () => {
      expect(
        await runProgram(`
(import image)
(font "Arial")
(font "Arial" "serif")
(font "Arial" "serif" #t)
`),
      ).toEqual([
        'Runtime error [2:1-2:14]: Arity mismatch in function call: expected 4 arguments, got 1',
        'Runtime error [3:1-3:22]: Arity mismatch in function call: expected 4 arguments, got 2',
        'Runtime error [4:1-4:25]: Arity mismatch in function call: expected 4 arguments, got 3',
      ])
    })
  })
})

describe('drawing', () => {
  describe('canvas?', () => {
    test('is true for a value made by make-canvas', async () => {
      expect(
        await runProgram(`
(import image)
(import canvas)
(canvas? (make-canvas 10 10))
`),
      ).toEqual(['#t'])
    })

    test('is false for non-canvas values', async () => {
      expect(
        await runProgram(`
(import image)
(canvas? (rectangle 10 10 "solid" "red"))
(canvas? 5)
(canvas? "canvas")
`),
      ).toEqual(['#f', '#f', '#f'])
    })
  })

  describe('html?', () => {
    test('is true for a value made by tag, and for a canvas', async () => {
      expect(
        await runProgram(`
(import image)
(import canvas)
(import html)
(html? (tag "div"))
(html? (make-canvas 10 10))
`),
      ).toEqual(['#t', '#t'])
    })

    test('is false for non-element values', async () => {
      expect(
        await runProgram(`
(import image)
(html? (rectangle 10 10 "solid" "red"))
(html? 5)
`),
      ).toEqual(['#f', '#f'])
    })
  })

  describe('image? and shape?', () => {
    test('are aliases, true for any drawing', async () => {
      expect(
        await runProgram(`
(import image)
(image? (rectangle 10 10 "solid" "red"))
(shape? (rectangle 10 10 "solid" "red"))
(image? (beside (rectangle 10 10 "solid" "red") (circle 5 "solid" "blue")))
`),
      ).toEqual(['#t', '#t', '#t'])
    })

    test('are false for non-drawing values', async () => {
      expect(
        await runProgram(`
(import image)
(image? 5)
(shape? "shape")
(image? (font "Arial" "sans-serif" #f #f))
`),
      ).toEqual(['#f', '#f', '#f'])
    })
  })

  describe('ellipse', () => {
    // N.B., the docstring declares `fill` as `boolean?` here (unlike the
    // other shape constructors below, which declare it `string?`), and the
    // contract checker enforces exactly that.
    test('constructs a solid or outlined ellipse', async () => {
      expect(
        await runProgram(`
(import image)
(ellipse 10 20 #t "red")
(ellipse 10 20 #f "blue")
`),
      ).toEqual([
        '(ellipse 10 20 #t (rgba 255 0 0 255))',
        '(ellipse 10 20 #f (rgba 0 0 255 255))',
      ])
    })

    test('rejects a non-boolean fill or a non-string color', async () => {
      expect(
        await runProgram(`
(import image)
(ellipse 10 20 "solid" "red")
(ellipse 10 20 #t 5)
`),
      ).toEqual([
        'Runtime error [322:1-322:41]: (error) expected a boolean, received string',
        'Runtime error [322:1-322:41]: (error) expected a string, received number',
      ])
    })

    test('requires four arguments', async () => {
      expect(
        await runProgram(`
(import image)
(ellipse 10 20 #t)
`),
      ).toEqual([
        'Runtime error [2:1-2:18]: Arity mismatch in function call: expected 4 arguments, got 3',
      ])
    })
  })

  describe('circle', () => {
    test('constructs an ellipse with equal width and height', async () => {
      expect(
        await runProgram(`
(import image)
(circle 5 "solid" "red")
(circle 5 "outline" "blue")
`),
      ).toEqual([
        '(ellipse 10 10 "solid" (rgba 255 0 0 255))',
        '(ellipse 10 10 "outline" (rgba 0 0 255 255))',
      ])
    })

    test('does not validate that radius is non-negative', async () => {
      expect(
        await runProgram(`
(import image)
(circle -5 "solid" "red")
(circle 0 "solid" "red")
`),
      ).toEqual([
        '(ellipse -10 -10 "solid" (rgba 255 0 0 255))',
        '(ellipse 0 0 "solid" (rgba 255 0 0 255))',
      ])
    })

    test('rejects a non-string fill', async () => {
      expect(
        await runProgram(`
(import image)
(circle 5 #t "red")
`),
      ).toEqual([
        'Runtime error [332:1-332:39]: (error) expected a string, received boolean',
      ])
    })
  })

  describe('rectangle', () => {
    test('constructs a solid or outlined rectangle', async () => {
      expect(
        await runProgram(`
(import image)
(rectangle 10 20 "solid" "red")
(rectangle 10 20 "outline" "blue")
`),
      ).toEqual([
        '(rectangle 10 20 "solid" (rgba 255 0 0 255))',
        '(rectangle 10 20 "outline" (rgba 0 0 255 255))',
      ])
    })

    test('accepts any string as fill, not just "solid"/"outline"', async () => {
      expect(
        await runProgram(`
(import image)
(rectangle 10 20 "diagonal" "red")
`),
      ).toEqual(['(rectangle 10 20 "diagonal" (rgba 255 0 0 255))'])
    })

    test('rejects a non-numeric width, an unknown color name, or a non-string color', async () => {
      expect(
        await runProgram(`
(import image)
(rectangle "a" 20 "solid" "red")
(rectangle 10 20 "solid" "not-a-color")
(rectangle 10 20 "solid" 5)
`),
      ).toEqual([
        'Runtime error [343:1-343:45]: (error) expected a number, received string',
        'Runtime error [3:1-3:39]: (rectangle) color-name->rgb: unknown color name not-a-color',
        'Runtime error [343:1-343:45]: (error) expected a string, received number',
      ])
    })
  })

  describe('square', () => {
    test('constructs a rectangle with equal width and height', async () => {
      expect(
        await runProgram(`
(import image)
(square 10 "solid" "red")
`),
      ).toEqual(['(rectangle 10 10 "solid" (rgba 255 0 0 255))'])
    })

    test('requires three arguments', async () => {
      expect(
        await runProgram(`
(import image)
(square 10 "solid")
`),
      ).toEqual([
        'Runtime error [2:1-2:19]: Arity mismatch in function call: expected 3 arguments, got 2',
      ])
    })
  })

  describe('triangle', () => {
    test('constructs an equilateral triangle, deriving height from length', async () => {
      expect(
        await runProgram(`
(import image)
(triangle 10 "solid" "red")
`),
      ).toEqual(['(triangle 10 8.660254037844386 "solid" (rgba 255 0 0 255))'])
    })

    test('a zero length is a degenerate, zero-size triangle', async () => {
      expect(
        await runProgram(`
(import image)
(triangle 0 "solid" "red")
`),
      ).toEqual(['(triangle 0 0 "solid" (rgba 255 0 0 255))'])
    })

    test('rejects a non-numeric length', async () => {
      expect(
        await runProgram(`
(import image)
(triangle "a" "solid" "red")
`),
      ).toEqual([
        'Runtime error [363:1-363:43]: (error) expected a number, received string',
      ])
    })
  })

  describe('isosceles-triangle', () => {
    test('constructs a triangle with independent width and height', async () => {
      expect(
        await runProgram(`
(import image)
(isosceles-triangle 10 20 "solid" "red")
`),
      ).toEqual(['(triangle 10 20 "solid" (rgba 255 0 0 255))'])
    })

    test('rejects a non-numeric height', async () => {
      expect(
        await runProgram(`
(import image)
(isosceles-triangle 10 "a" "solid" "red")
`),
      ).toEqual([
        'Runtime error [374:1-374:62]: (error) expected a number, received string',
      ])
    })
  })

  describe('path', () => {
    test('connects the given points with straight lines', async () => {
      expect(
        await runProgram(`
(import image)
(path 10 10 (list (pair 0 0) (pair 5 5) (pair 10 0)) "solid" "red")
`),
      ).toEqual([
        '(path 10 10 (vector (vector 0 0) (vector 5 5) (vector 10 0)) "solid" (rgba 255 0 0 255))',
      ])
    })

    test('an empty point list is still a valid, empty path', async () => {
      expect(
        await runProgram(`
(import image)
(path 0 0 (list) "solid" "red")
`),
      ).toEqual(['(path 0 0 (vector) "solid" (rgba 255 0 0 255))'])
    })

    test('width and height are recorded as given, independent of the points', async () => {
      expect(
        await runProgram(`
(import image)
(image-width (path 100 200 (list (pair 0 0)) "solid" "red"))
(image-height (path 100 200 (list (pair 0 0)) "solid" "red"))
`),
      ).toEqual(['100', '200'])
    })

    test('rejects a non-list points argument or a non-numeric dimension', async () => {
      expect(
        await runProgram(`
(import image)
(path 10 10 5 "solid" "red")
(path "a" 10 (list) "solid" "red")
`),
      ).toEqual([
        'Runtime error [387:1-387:35]: (error) expected a list, received number',
        'Runtime error [387:1-387:35]: (error) expected a number, received string',
      ])
    })
  })

  describe('beside', () => {
    test('sums widths and takes the max height, aligned to center', async () => {
      expect(
        await runProgram(`
(import image)
(beside (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
`),
      ).toEqual([
        '(beside "center" 14 10 (vector (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255))))',
      ])
    })

    test('a single drawing is unaffected', async () => {
      expect(
        await runProgram(`
(import image)
(image-width (beside (rectangle 10 10 "solid" "red")))
(image-height (beside (rectangle 10 10 "solid" "red")))
`),
      ).toEqual(['10', '10'])
    })

    test('with no drawings, width is 0 and height is -infinity', async () => {
      expect(
        await runProgram(`
(import image)
(beside)
`),
      ).toEqual(['(beside "center" 0 -Infinity (vector))'])
    })

    test('rejects a non-image argument', async () => {
      expect(
        await runProgram(`
(import image)
(beside (rectangle 10 10 "solid" "red") "not-a-drawing")
`),
      ).toEqual([
        'Runtime error [393:1-393:39]: (error) expected every value of d1 to be an image, but at least one was not',
      ])
    })
  })

  describe('beside/align', () => {
    test('records the given alignment', async () => {
      expect(
        await runProgram(`
(import image)
(beside/align "top" (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
`),
      ).toEqual([
        '(beside "top" 14 10 (vector (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255))))',
      ])
    })

    test('rejects a non-string alignment', async () => {
      expect(
        await runProgram(`
(import image)
(beside/align 5 (rectangle 10 10 "solid" "red"))
`),
      ).toEqual([
        'Runtime error [401:1-401:50]: (error) expected a string, received number',
      ])
    })
  })

  describe('above', () => {
    test('takes the max width and sums heights, aligned to middle', async () => {
      expect(
        await runProgram(`
(import image)
(above (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
`),
      ).toEqual([
        '(above "middle" 10 16 (vector (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255))))',
      ])
    })

    test('with no drawings, height is 0 and width is -infinity', async () => {
      expect(
        await runProgram(`
(import image)
(above)
`),
      ).toEqual(['(above "middle" -Infinity 0 (vector))'])
    })
  })

  describe('above/align', () => {
    test('records the given alignment', async () => {
      expect(
        await runProgram(`
(import image)
(above/align "left" (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
`),
      ).toEqual([
        '(above "left" 10 16 (vector (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255))))',
      ])
    })

    test('rejects a non-string alignment', async () => {
      expect(
        await runProgram(`
(import image)
(above/align 5 (rectangle 10 10 "solid" "red"))
`),
      ).toEqual([
        'Runtime error [415:1-415:48]: (error) expected a string, received number',
      ])
    })
  })

  describe('overlay', () => {
    test('takes the max width and max height, aligned to middle/center', async () => {
      expect(
        await runProgram(`
(import image)
(overlay (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
`),
      ).toEqual([
        '(overlay "middle" "center" 10 10 (vector (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255))))',
      ])
    })

    test('with no drawings, both dimensions are -infinity', async () => {
      expect(
        await runProgram(`
(import image)
(overlay)
`),
      ).toEqual(['(overlay "middle" "center" -Infinity -Infinity (vector))'])
    })
  })

  describe('overlay/align', () => {
    test('records the given x- and y-alignments', async () => {
      expect(
        await runProgram(`
(import image)
(overlay/align "left" "top" (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
`),
      ).toEqual([
        '(overlay "left" "top" 10 10 (vector (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255))))',
      ])
    })

    test('rejects a non-string alignment', async () => {
      expect(
        await runProgram(`
(import image)
(overlay/align "left" 5 (rectangle 10 10 "solid" "red"))
`),
      ).toEqual([
        'Runtime error [431:1-431:52]: (error) expected a string, received number',
      ])
    })
  })

  describe('overlay/offset', () => {
    test('grows the bounding box by the offset, in the direction of the offset', async () => {
      expect(
        await runProgram(`
(import image)
(overlay/offset 3 4 (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
(overlay/offset -3 -4 (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue"))
`),
      ).toEqual([
        '(overlayOffset 3 4 10 10 (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255)))',
        '(overlayOffset -3 -4 13 14 (rectangle 10 10 "solid" (rgba 255 0 0 255)) (ellipse 4 6 #t (rgba 0 0 255 255)))',
      ])
    })

    test('a zero offset just stacks the two drawings', async () => {
      expect(
        await runProgram(`
(import image)
(overlay/offset 0 0 (rectangle 10 10 "solid" "red") (rectangle 10 10 "solid" "blue"))
`),
      ).toEqual([
        '(overlayOffset 0 0 10 10 (rectangle 10 10 "solid" (rgba 255 0 0 255)) (rectangle 10 10 "solid" (rgba 0 0 255 255)))',
      ])
    })

    test('rejects a non-image d1 or d2', async () => {
      expect(
        await runProgram(`
(import image)
(overlay/offset 0 0 "not-a-drawing" (rectangle 10 10 "solid" "red"))
`),
      ).toEqual([
        'Runtime error [440:1-440:54]: (error) expected an image, received string',
      ])
    })
  })

  describe('rotate', () => {
    test('grows the bounding box to fit the rotated drawing', async () => {
      expect(
        await runProgram(`
(import image)
(rotate 45 (rectangle 10 10 "solid" "red"))
`),
      ).toEqual([
        '(rotate 14.142135623730951 14.142135623730951 7.071067811865475 0 45 (rectangle 10 10 "solid" (rgba 255 0 0 255)))',
      ])
    })

    test('a 0-degree rotation leaves the bounding box unchanged', async () => {
      expect(
        await runProgram(`
(import image)
(rotate 0 (rectangle 10 10 "solid" "red"))
`),
      ).toEqual(['(rotate 10 10 0 0 0 (rectangle 10 10 "solid" (rgba 255 0 0 255)))'])
    })

    test('rejects a non-numeric angle or a non-image drawing', async () => {
      expect(
        await runProgram(`
(import image)
(rotate "a" (rectangle 10 10 "solid" "red"))
(rotate 45 5)
`),
      ).toEqual([
        'Runtime error [448:1-448:39]: (error) expected a number, received string',
        'Runtime error [448:1-448:39]: (error) expected an image, received number',
      ])
    })
  })

  describe('with-dash', () => {
    test('records the dash spec and inherits the size of the wrapped drawing', async () => {
      expect(
        await runProgram(`
(import image)
(with-dash (list 4 2) (rectangle 10 10 "solid" "red"))
`),
      ).toEqual([
        '(withDash (list 4 2) (rectangle 10 10 "solid" (rgba 255 0 0 255)) 10 10)',
      ])
    })

    test('an empty dash spec is the empty list', async () => {
      expect(
        await runProgram(`
(import image)
(with-dash (list) (rectangle 10 10 "solid" "red"))
`),
      ).toEqual([
        '(withDash null (rectangle 10 10 "solid" (rgba 255 0 0 255)) 10 10)',
      ])
    })

    test('rejects a non-list dash spec or a non-image drawing', async () => {
      expect(
        await runProgram(`
(import image)
(with-dash 5 (rectangle 10 10 "solid" "red"))
(with-dash (list 4 2) 5)
`),
      ).toEqual([
        'Runtime error [456:1-456:44]: (error) expected a list, received number',
        'Runtime error [456:1-456:44]: (error) expected an image, received number',
      ])
    })
  })

  describe('text', () => {
    // vitest-canvas-mock's measureText reports width = text length and zero
    // ascent/descent, so "hi" is a deterministic 2x1 box under jsdom.
    test('constructs a text drawing, defaulting the font to Arial', async () => {
      expect(
        await runProgram(`
(import image)
(text "hi" 12 "black")
`),
      ).toEqual(['(text 2 1 "hi" 12 (rgba 0 0 0 255) (font "Arial" "sans-serif" #f #f))'])
    })

    test('accepts a font as an optional fourth argument', async () => {
      expect(
        await runProgram(`
(import image)
(text "hi" 12 "black" (font "Georgia" "serif" #t #f))
`),
      ).toEqual(['(text 2 1 "hi" 12 (rgba 0 0 0 255) (font "Georgia" "serif" #t #f))'])
    })

    // A non-font fourth argument fails the image_fontQ guard, so it is silently
    // dropped rather than rejected and the default font is kept.
    test('silently ignores a non-font fourth argument', async () => {
      expect(
        await runProgram(`
(import image)
(text "hi" 12 "black" 5)
`),
      ).toEqual(['(text 2 1 "hi" 12 (rgba 0 0 0 255) (font "Arial" "sans-serif" #f #f))'])
    })

    test('rejects more than one argument after the color', async () => {
      expect(
        await runProgram(`
(import image)
(text "hi" 12 "black" (font "Arial" "sans-serif" #f #f) 9)
`),
      ).toEqual([
        'Runtime error [2:1-2:58]: (text) wrong number of arguments to text provided. Expected 3 or 4, received 5.',
      ])
    })
  })

  describe('solid-square and outlined-square', () => {
    test('are equivalent to square with a fixed fill', async () => {
      expect(
        await runProgram(`
(import image)
(solid-square 10 "red")
(outlined-square 10 "red")
`),
      ).toEqual([
        '(rectangle 10 10 "solid" (rgba 255 0 0 255))',
        '(rectangle 10 10 "outline" (rgba 255 0 0 255))',
      ])
    })

    test('reject a non-string color', async () => {
      expect(
        await runProgram(`
(import image)
(solid-square 10 5)
(outlined-square 10 5)
`),
      ).toEqual([
        'Runtime error [475:1-475:50]: (error) expected a string, received number',
        'Runtime error [483:1-483:56]: (error) expected a string, received number',
      ])
    })
  })

  describe('solid-rectangle and outlined-rectangle', () => {
    test('are equivalent to rectangle with a fixed fill', async () => {
      expect(
        await runProgram(`
(import image)
(solid-rectangle 10 20 "red")
(outlined-rectangle 10 20 "red")
`),
      ).toEqual([
        '(rectangle 10 20 "solid" (rgba 255 0 0 255))',
        '(rectangle 10 20 "outline" (rgba 255 0 0 255))',
      ])
    })

    test('reject a non-numeric width or height', async () => {
      expect(
        await runProgram(`
(import image)
(solid-rectangle "a" 20 "red")
(outlined-rectangle 10 "a" "red")
`),
      ).toEqual([
        'Runtime error [492:1-492:56]: (error) expected a number, received string',
        'Runtime error [501:1-501:62]: (error) expected a number, received string',
      ])
    })
  })

  describe('solid-circle and outlined-circle', () => {
    test('are equivalent to circle with a fixed fill', async () => {
      expect(
        await runProgram(`
(import image)
(solid-circle 5 "red")
(outlined-circle 5 "red")
`),
      ).toEqual([
        '(ellipse 10 10 "solid" (rgba 255 0 0 255))',
        '(ellipse 10 10 "outline" (rgba 255 0 0 255))',
      ])
    })

    test('reject a non-string color', async () => {
      expect(
        await runProgram(`
(import image)
(solid-circle 5 5)
`),
      ).toEqual([
        'Runtime error [509:1-509:50]: (error) expected a string, received number',
      ])
    })
  })

  describe('solid-ellipse and outlined-ellipse', () => {
    test('are equivalent to ellipse with a fixed fill', async () => {
      expect(
        await runProgram(`
(import image)
(solid-ellipse 10 20 "red")
(outlined-ellipse 10 20 "red")
`),
      ).toEqual([
        '(ellipse 10 20 "solid" (rgba 255 0 0 255))',
        '(ellipse 10 20 "outline" (rgba 255 0 0 255))',
      ])
    })

    test('reject a non-numeric width or height', async () => {
      expect(
        await runProgram(`
(import image)
(solid-ellipse "a" 20 "red")
`),
      ).toEqual([
        'Runtime error [525:1-525:52]: (error) expected an integer, received string',
      ])
    })
  })

  describe('solid-triangle and outlined-triangle', () => {
    test('are equivalent to triangle with a fixed fill', async () => {
      expect(
        await runProgram(`
(import image)
(solid-triangle 10 "red")
(outlined-triangle 10 "red")
`),
      ).toEqual([
        '(triangle 10 8.660254037844386 "solid" (rgba 255 0 0 255))',
        '(triangle 10 8.660254037844386 "outline" (rgba 255 0 0 255))',
      ])
    })

    test('reject a non-numeric length', async () => {
      expect(
        await runProgram(`
(import image)
(solid-triangle "a" "red")
`),
      ).toEqual([
        'Runtime error [541:1-541:54]: (error) expected a number, received string',
      ])
    })
  })

  describe('solid-isosceles-triangle and outlined-isosceles-triangle', () => {
    test('are equivalent to isosceles-triangle with a fixed fill', async () => {
      expect(
        await runProgram(`
(import image)
(solid-isosceles-triangle 10 20 "red")
(outlined-isosceles-triangle 10 20 "red")
`),
      ).toEqual([
        '(triangle 10 20 "solid" (rgba 255 0 0 255))',
        '(triangle 10 20 "outline" (rgba 255 0 0 255))',
      ])
    })

    test('reject a non-numeric width or height', async () => {
      expect(
        await runProgram(`
(import image)
(solid-isosceles-triangle 10 "a" "red")
`),
      ).toEqual([
        'Runtime error [558:1-558:73]: (error) expected a number, received string',
      ])
    })
  })

  describe('image-width', () => {
    test('reads the width field of a drawing', async () => {
      expect(
        await runProgram(`
(import image)
(image-width (rectangle 10 20 "solid" "red"))
(image-width (beside (rectangle 10 10 "solid" "red") (rectangle 5 5 "solid" "blue")))
`),
      ).toEqual(['10', '15'])
    })

    test('rejects a non-image value, including a canvas', async () => {
      expect(
        await runProgram(`
(import image)
(import canvas)
(image-width 5)
(image-width (make-canvas 20 15))
`),
      ).toEqual([
        'Runtime error [573:1-573:48]: (error) expected an image, received number',
        'Runtime error [573:1-573:48]: (error) expected an image, received object',
      ])
    })
  })

  describe('image-height', () => {
    test('reads the height field of a drawing', async () => {
      expect(
        await runProgram(`
(import image)
(image-height (rectangle 10 20 "solid" "red"))
(image-height (above (rectangle 10 10 "solid" "red") (rectangle 5 5 "solid" "blue")))
`),
      ).toEqual(['20', '15'])
    })

    test('rejects a non-image value, including a canvas', async () => {
      expect(
        await runProgram(`
(import image)
(import canvas)
(image-height 5)
(image-height (make-canvas 20 15))
`),
      ).toEqual([
        'Runtime error [579:1-579:50]: (error) expected an image, received number',
        'Runtime error [579:1-579:50]: (error) expected an image, received object',
      ])
    })
  })

  describe('image-color', () => {
    test('reads the color of a leaf drawing', async () => {
      expect(
        await runProgram(`
(import image)
(image-color (rectangle 10 20 "solid" "red"))
`),
      ).toEqual(['(rgba 255 0 0 255)'])
    })

    test('averages the colors of the children of a composite drawing', async () => {
      expect(
        await runProgram(`
(import image)
(image-color (beside (rectangle 10 10 "solid" "red") (rectangle 5 5 "solid" "blue")))
(image-color (overlay/offset 3 4 (rectangle 10 10 "solid" "red") (rectangle 5 5 "solid" "blue")))
`),
      ).toEqual(['(rgba 127.5 0 127.5 255)', '(rgba 127.5 0 127.5 255)'])
    })

    test('passes through the wrapped color for rotate and with-dash', async () => {
      expect(
        await runProgram(`
(import image)
(image-color (rotate 45 (rectangle 10 10 "solid" "red")))
(image-color (with-dash (list 4 2) (rectangle 10 10 "solid" "red")))
`),
      ).toEqual(['(rgba 255 0 0 255)', '(rgba 255 0 0 255)'])
    })

    test('reads the color of a text leaf', async () => {
      expect(
        await runProgram(`
(import image)
(image-color (text "hi" 12 "black"))
`),
      ).toEqual(['(rgba 0 0 0 255)'])
    })

    test('rejects a non-image value', async () => {
      expect(
        await runProgram(`
(import image)
(image-color 5)
`),
      ).toEqual([
        'Runtime error [585:1-585:48]: (error) expected an image, received number',
      ])
    })
  })

  describe('image-recolor', () => {
    test('replaces the color of a leaf drawing, accepting a string or rgb color', async () => {
      expect(
        await runProgram(`
(import image)
(image-recolor (rectangle 10 10 "solid" "red") "blue")
(image-recolor (rectangle 10 10 "solid" "red") (rgb 1 2 3))
`),
      ).toEqual([
        '(rectangle 10 10 "solid" (rgba 0 0 255 255))',
        '(rectangle 10 10 "solid" (rgba 1 2 3 255))',
      ])
    })

    test('recursively recolors every leaf of a composite drawing, preserving structure', async () => {
      expect(
        await runProgram(`
(import image)
(image-recolor (beside (rectangle 10 10 "solid" "red") (ellipse 4 6 #t "blue")) "green")
(image-recolor (rotate 45 (rectangle 10 10 "solid" "red")) "green")
(image-recolor (with-dash (list 4 2) (rectangle 10 10 "solid" "red")) "green")
`),
      ).toEqual([
        '(beside "center" 14 10 (vector (rectangle 10 10 "solid" (rgba 0 128 0 255)) (ellipse 4 6 #t (rgba 0 128 0 255))))',
        '(rotate 14.142135623730951 14.142135623730951 7.071067811865475 0 45 (rectangle 10 10 "solid" (rgba 0 128 0 255)))',
        '(withDash (list 4 2) (rectangle 10 10 "solid" (rgba 0 128 0 255)) 10 10)',
      ])
    })

    // The text branch rebuilds the drawing with drawing.color, not the
    // requested color, so a text leaf comes back with its original color.
    test('leaves a text leaf with its original color', async () => {
      expect(
        await runProgram(`
(import image)
(image-recolor (text "hi" 12 "black") "green")
`),
      ).toEqual(['(text 2 1 "hi" 12 (rgba 0 0 0 255) (font "Arial" "sans-serif" #f #f))'])
    })

    test('rejects a non-image value or a non-color', async () => {
      expect(
        await runProgram(`
(import image)
(image-recolor 5 "red")
(image-recolor (rectangle 10 10 "solid" "red") 5)
`),
      ).toEqual([
        'Runtime error [592:1-592:52]: (error) expected an image, received number',
        'Runtime error [592:1-592:52]: (error) expected a color, received number',
      ])
    })
  })
})

describe('image transforms', () => {
  describe('with-image-file', () => {
    test('wraps the callback in a reactive-image-file struct', async () => {
      expect(
        await runProgram(`
(import image)
(with-image-file (lambda (x) x))
(with-image-file car)
`),
      ).toEqual([
        '(reactive-image-file [Function: ##anonymous##])',
        '(reactive-image-file [Function: car])',
      ])
    })

    // with-image-file is a pure struct constructor -- it never calls the
    // callback (the loading/invocation logic lives in the file-chooser
    // renderer), so a callback that would error if run must still produce
    // just the struct here, not a runtime error.
    test('never invokes the callback', async () => {
      expect(
        await runProgram(`
(import image)
(with-image-file (lambda (x) (error "should never run")))
`),
      ).toEqual(['(reactive-image-file [Function: ##anonymous##])'])
    })

    test('rejects a non-procedure callback', async () => {
      expect(
        await runProgram(`
(import image)
(with-image-file 5)
`),
      ).toEqual([
        'Runtime error [610:1-610:55]: (error) expected a procedure, received number',
      ])
    })

    test('requires exactly one argument', async () => {
      expect(
        await runProgram(`
(import image)
(with-image-file)
(with-image-file car car)
`),
      ).toEqual([
        'Runtime error [2:1-2:17]: Arity mismatch in function call: expected 1 arguments, got 0',
        'Runtime error [3:1-3:25]: Arity mismatch in function call: expected 1 arguments, got 2',
      ])
    })
  })

  // reactive-image-file? has no Scheme-level binding in image.scm (the predicate
  // is only wired up as an html/vue custom-render check), so it is exercised by
  // importing the JS predicate directly rather than through runProgram.
  describe('reactive-image-file?', () => {
    const dummyScamperFn = ((..._args: L.Value[]) => undefined) as L.ScamperFn

    test('is true for a value made by with-image-file', () => {
      expect(image_isReactiveImageFile(image_withImageFile(dummyScamperFn))).toBe(true)
    })

    test('is false for non-reactive-image-file values', () => {
      expect(image_isReactiveImageFile(5)).toBe(false)
      expect(image_isReactiveImageFile('reactive-image-file')).toBe(false)
    })
  })
})

// Rotating a drawing forces getDrawingPoints to compute a point set for the
// wrapped shape (one switch case per drawing kind), then a rotated bounding
// box. Asserting the box is a positive size exercises each case without
// depending on the exact irrational post-rotation dimensions.
describe('getDrawingPoints (via rotate)', () => {
  async function rotatedIsPositive(shape: string): Promise<string[]> {
    return runProgram(`
(import image)
(> (image-width (rotate 45 ${shape})) 0)
(> (image-height (rotate 45 ${shape})) 0)
`)
  }

  test('ellipse', async () => {
    expect(await rotatedIsPositive('(ellipse 10 20 #t "red")')).toEqual(['#t', '#t'])
  })
  test('rectangle', async () => {
    expect(await rotatedIsPositive('(rectangle 10 20 "solid" "red")')).toEqual(['#t', '#t'])
  })
  test('triangle', async () => {
    expect(await rotatedIsPositive('(triangle 10 "solid" "red")')).toEqual(['#t', '#t'])
  })
  test('path', async () => {
    expect(await rotatedIsPositive('(path 10 10 (list (pair 0 0) (pair 5 5) (pair 10 0)) "solid" "red")')).toEqual(['#t', '#t'])
  })
  test('beside', async () => {
    expect(await rotatedIsPositive('(beside (square 5 "solid" "red") (square 8 "solid" "blue"))')).toEqual(['#t', '#t'])
  })
  // Rotating an `above` drawing currently crashes: getDrawingPoints' `above`
  // case increments `xOffset` (a copy-paste slip from the `beside` case) which
  // isn't in scope there, so it throws a ReferenceError
  // (github.com/slag-plt/scamper#253). Asserting the throw documents the bug
  // and covers the reachable portion of the branch.
  test('above (broken -- see #253)', async () => {
    const [err] = await runProgram(`
(import image)
(image-width (rotate 45 (above (square 5 "solid" "red") (square 8 "solid" "blue"))))
`)
    expect(err).toContain("Cannot access 'xOffset' before initialization")
  })
  test('overlay', async () => {
    expect(await rotatedIsPositive('(overlay (square 5 "solid" "red") (square 8 "solid" "blue"))')).toEqual(['#t', '#t'])
  })
  test('overlay/offset', async () => {
    expect(await rotatedIsPositive('(overlay/offset 3 4 (square 5 "solid" "red") (square 8 "solid" "blue"))')).toEqual(['#t', '#t'])
  })
  test('nested rotate', async () => {
    expect(await rotatedIsPositive('(rotate 30 (rectangle 10 10 "solid" "red"))')).toEqual(['#t', '#t'])
  })
  test('with-dash', async () => {
    expect(await rotatedIsPositive('(with-dash (list 5 5) (rectangle 10 20 "solid" "red"))')).toEqual(['#t', '#t'])
  })
  test('text', async () => {
    expect(await rotatedIsPositive('(text "hi" 20 "black")')).toEqual(['#t', '#t'])
  })
})

describe('image-color of composite drawings', () => {
  test('beside/above/overlay average their children (uniform colour)', async () => {
    expect(await runProgram(`
(import image)
(image-color (beside (square 10 "solid" "red") (square 10 "solid" "red")))
(image-color (above (square 10 "solid" "red") (square 10 "solid" "red")))
(image-color (overlay (square 10 "solid" "red") (square 10 "solid" "red")))
`)).toEqual([
      '(rgba 255 0 0 255)',
      '(rgba 255 0 0 255)',
      '(rgba 255 0 0 255)',
    ])
  })
  test('overlay/offset averages its two drawings', async () => {
    expect(await runProgram(`
(import image)
(image-color (overlay/offset 2 2 (square 10 "solid" "red") (square 10 "solid" "red")))
`)).toEqual(['(rgba 255 0 0 255)'])
  })
  test('rotate and with-dash delegate to the wrapped drawing', async () => {
    expect(await runProgram(`
(import image)
(image-color (rotate 45 (square 10 "solid" "red")))
(image-color (with-dash (list 5 5) (square 10 "solid" "red")))
`)).toEqual(['(rgba 255 0 0 255)', '(rgba 255 0 0 255)'])
  })
  test('text returns its own colour', async () => {
    expect(await runProgram(`
(import image)
(image-color (text "hi" 20 "red"))
`)).toEqual(['(rgba 255 0 0 255)'])
  })
})

describe('image-recolor of composite drawings', () => {
  // image-recolor rebuilds each drawing kind with the new colour; checking the
  // recoloured drawing's colour confirms the rebuild reached every leaf.
  test('leaf kinds (triangle, path)', async () => {
    expect(await runProgram(`
(import image)
(image-color (image-recolor (triangle 10 "solid" "red") "green"))
(image-color (image-recolor (path 10 10 (list (pair 0 0) (pair 5 5)) "solid" "red") "green"))
`)).toEqual(['(rgba 0 128 0 255)', '(rgba 0 128 0 255)'])
  })
  test('aggregate kinds (above, overlay, overlay/offset)', async () => {
    expect(await runProgram(`
(import image)
(image-color (image-recolor (above (square 5 "solid" "red") (square 8 "solid" "red")) "green"))
(image-color (image-recolor (overlay (square 5 "solid" "red") (square 8 "solid" "red")) "green"))
(image-color (image-recolor (overlay/offset 2 2 (square 5 "solid" "red") (square 8 "solid" "red")) "green"))
`)).toEqual(['(rgba 0 128 0 255)', '(rgba 0 128 0 255)', '(rgba 0 128 0 255)'])
  })
  test('wrapper kinds (rotate, with-dash)', async () => {
    expect(await runProgram(`
(import image)
(image-color (image-recolor (rotate 45 (square 5 "solid" "red")) "green"))
(image-color (image-recolor (with-dash (list 5 5) (square 5 "solid" "red")) "green"))
`)).toEqual(['(rgba 0 128 0 255)', '(rgba 0 128 0 255)'])
  })
})

describe('overlay/offset dimension branches', () => {
  // The width/height are computed by cases on whether d1 is larger than d2 and
  // whether the offset is positive; cover the smaller-d1, positive-offset case
  // (distinct from the larger-d1 cases the constructor tests already hit).
  test('smaller first drawing with positive offset stays positive', async () => {
    expect(await runProgram(`
(import image)
(> (image-width (overlay/offset 3 4 (square 5 "solid" "red") (square 20 "solid" "blue"))) 0)
(> (image-height (overlay/offset 3 4 (square 5 "solid" "red") (square 20 "solid" "blue"))) 0)
`)).toEqual(['#t', '#t'])
  })
})

describe('image-width / image-height of a canvas', () => {
  // A canvas is not a drawing struct, so image-width/height fall through to the
  // HTMLCanvasElement branch. The Scamper-level contract rejects a raw canvas,
  // so exercise that fallback by calling the JS functions directly.
  test('return the canvas dimensions', () => {
    const canvas = document.createElement('canvas')
    canvas.width = 30
    canvas.height = 20
    expect(image_imageWidth(canvas as unknown as never)).toBe(30)
    expect(image_imageHeight(canvas as unknown as never)).toBe(20)
  })
})

describe('colour edge cases', () => {
  // Too few args trip the outer call-arity contract; too many (5) reach
  // image_hsv's own "expects 3 or 4 arguments" guard.
  test('hsv rejects too many arguments', async () => {
    expect(await runProgram(`
(import image)
(hsv 1 2 3 4 5)
`)).toEqual(['Runtime error [2:1-2:15]: (hsv) hsv: expects 3 or 4 arguments, but got 5'])
  })
  test('rgb-hue of a grey colour is a hue in [0, 360)', async () => {
    expect(await runProgram(`
(import image)
(and (>= (rgb-hue (rgb 5 5 5)) 0) (< (rgb-hue (rgb 5 5 5)) 360))
`)).toEqual(['#t'])
  })
})
