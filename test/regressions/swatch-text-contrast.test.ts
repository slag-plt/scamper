import { expect, test } from 'vitest'
import { mount } from '@vue/test-utils'
import HtmlRenderer from '../../src/lpm/renderers/html'
import RgbRenderer from '../../src/js/image/renderers/RgbRenderer.vue'
import HsvRenderer from '../../src/js/image/renderers/HsvRenderer.vue'
import { Rgb, color_hsv, color_rgb } from '../../src/js/image/color'
// Registers the image library's HTML custom renderers on the shared
// HtmlRenderer singleton (see test/regressions/tracing-images.test.ts).
import '../../src/app/web/renderers'

// https://github.com/slag-plt/scamper/issues/456
//
// The label written across a color swatch was the color's pseudo-complement,
// which is a color in its own right -- green on purple -- and which inherited
// the swatch's alpha, so a nearly-transparent color got a nearly-transparent
// label. A label is legible or it is not: it should be plain black or plain
// white, chosen by the luminance of the swatch as it is actually painted,
// i.e. composited over its backing with its alpha.

/**
 * Canonicalizes a CSS color to 'black' / 'white' where it is one of them, so
 * a failure reports the offending color rather than a formatting difference.
 */
function ink(color: string): string {
  const normalized = color.trim().toLowerCase().replace(/\s+/g, '')
  if (['black', '#000', '#000000', 'rgb(0,0,0)', 'rgba(0,0,0,1)'].includes(normalized)) {
    return 'black'
  }
  if (['white', '#fff', '#ffffff', 'rgb(255,255,255)', 'rgba(255,255,255,1)'].includes(normalized)) {
    return 'white'
  }
  return color
}

/** The label color the HTML renderer paints over `swatch`. */
function htmlInk(swatch: Rgb): string {
  return ink(HtmlRenderer.render(swatch).style.color)
}

/** The root element a mounted renderer produced. */
function rootOf(wrapper: { element: Element }): HTMLElement {
  return wrapper.element as HTMLElement
}

/** The label color the Vue renderer paints over `swatch`. */
function vueInk(swatch: Rgb): string {
  return ink(rootOf(mount(RgbRenderer, { props: { value: swatch } })).style.color)
}

// The cases from the issue's screenshot, plus the two extremes. The expected
// ink is what the swatch reads as once composited over the (light) backing it
// is drawn on: a 25%-alpha red is pale pink, not red.
const cases: { what: string, swatch: Rgb, expected: string }[] = [
  { what: 'a saturated purple', swatch: color_rgb(128, 0, 128, 255), expected: 'white' },
  { what: 'a light yellow', swatch: color_rgb(255, 255, 128, 255), expected: 'black' },
  { what: 'a nearly-transparent red', swatch: color_rgb(255, 0, 0, 64), expected: 'black' },
  { what: 'a half-transparent red', swatch: color_rgb(255, 0, 0, 128), expected: 'black' },
  { what: 'a nearly-transparent magenta', swatch: color_rgb(255, 0, 255, 64), expected: 'black' },
  { what: 'pure black', swatch: color_rgb(0, 0, 0, 255), expected: 'white' },
  { what: 'pure white', swatch: color_rgb(255, 255, 255, 255), expected: 'black' },
]

test.each(cases)('the HTML swatch for $what is labelled in $expected', ({ swatch, expected }) => {
  expect(htmlInk(swatch)).toBe(expected)
})

test.each(cases)('the Vue swatch for $what is labelled in $expected', ({ swatch, expected }) => {
  expect(vueInk(swatch)).toBe(expected)
})

// An hsv swatch is the same swatch by another name, and shared the same bug.
test('an hsv swatch is labelled in black or white too', () => {
  // hsv(300, 100%, 50%) is the purple above.
  const purple = color_hsv(300, 100, 50, 255)
  expect(ink(HtmlRenderer.render(purple).style.color)).toBe('white')
  expect(ink(rootOf(mount(HsvRenderer, { props: { value: purple } })).style.color)).toBe('white')
})

// The other half of the fix: the swatch carries its own opaque ground, so a
// translucent color paints as what it looks like over that ground rather than
// letting the page show through. Without this the ink above would be chosen
// for a color the swatch is not actually painted in.
test('a translucent swatch is painted as its composite over white', () => {
  // A 25%-alpha red over white is a pale pink.
  const red = color_rgb(255, 0, 0, 64)
  const paleRed = 'rgb(255, 191, 191)'
  expect(HtmlRenderer.render(red).style.backgroundColor).toBe(paleRed)
  expect(rootOf(mount(RgbRenderer, { props: { value: red } })).style.backgroundColor).toBe(paleRed)
})
