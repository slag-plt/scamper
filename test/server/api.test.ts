import { describe, expect, test } from 'vitest'
import { API_ROOT, route } from '../../server/src/api'

describe('api routing', () => {
  test('every route is namespaced by API version', () => {
    // Old front-end releases stay live at their versioned URLs forever (see
    // scripts/deploy), so one server serves many client versions at once and
    // the prefix is what lets a breaking change ship beside the old one.
    expect(API_ROOT).toBe('/api/v1')
  })

  test('health reports ok', () => {
    expect(route(`${API_ROOT}/health`)).toEqual({
      status: 200,
      body: { status: 'ok', api: API_ROOT },
    })
  })

  test('an unclaimed path is a 404', () => {
    expect(route(`${API_ROOT}/nope`).status).toBe(404)
  })

  test('an unversioned path is not served', () => {
    expect(route('/health').status).toBe(404)
  })
})
