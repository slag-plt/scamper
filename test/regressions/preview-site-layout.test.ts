import { execFileSync } from 'node:child_process'
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import path from 'node:path'
import { afterEach, describe, expect, test } from 'vitest'

// https://github.com/slag-plt/scamper/issues/704
//
// The embeddable bundle used to be published as `<version>/scamper-embed.js`:
// the version in the *directory*, the filename identical for every release. So
// a shipped release's directory survived only to hold one file whose name said
// nothing about which release it came from, and the publish job had to reduce
// each directory in place rather than simply deleting it.
//
// The bundle now carries its version (`scamper-embed-<version>.js`) and lives at
// the root, which makes a shipped release's directory disposable.
//
// This logic *permanently deletes published files* -- a reading on another site
// loads one by URL, and `docs/releasing.md` spells out that removing a version's
// directory breaks any reading pinned to it. It used to live inline in
// .github/workflows/node.js.yml, where it runs only on main and so could not be
// exercised before it did damage. It is now scripts/compose-preview-site, and
// this is what exercises it.

const SCRIPT = path.resolve(import.meta.dirname, '../../scripts/compose-preview-site')

let scratch: string | null = null

afterEach(() => {
  if (scratch !== null) rmSync(scratch, { recursive: true, force: true })
  scratch = null
})

/** A file, with its parent directories. */
function put(root: string, rel: string, contents = 'x'): void {
  const full = path.join(root, rel)
  mkdirSync(path.dirname(full), { recursive: true })
  writeFileSync(full, contents)
}

/**
 * Builds a pages tree and a dist tree, runs the script over them, and returns
 * the pages directory it composed.
 *
 * @param pages what is already published, as paths relative to the site root
 * @param version the build being published, candidate suffix and all
 * @param release that version with any candidate suffix stripped
 */
function compose(
  pages: Record<string, string>,
  version: string,
  release: string,
): string {
  scratch = mkdtempSync(path.join(tmpdir(), 'scamper-pages-'))
  const site = path.join(scratch, 'site')
  const dist = path.join(scratch, 'dist')
  mkdirSync(site, { recursive: true })

  for (const [rel, contents] of Object.entries(pages)) put(site, rel, contents)

  // What `npm run build` leaves: the versioned bundle, the shim that keeps the
  // old name working, and a page's worth of everything else.
  put(dist, `scamper-embed-${version}.js`, `bundle ${version}`)
  put(dist, 'scamper-embed.js', `import './scamper-embed-${version}.js'\n`)
  put(dist, 'index.html', '<!doctype html>')
  put(dist, 'assets/scamper-ide-x.js', 'chunk')

  execFileSync(SCRIPT, [site, dist, version, release], { encoding: 'utf-8' })
  return site
}

describe('the preview site keeps one bundle per release, at its root', () => {
  test('a release places its bundle at the root as well as in its directory', () => {
    const site = compose({}, '4.8.0', '4.8.0')

    // The flat URL has to work the moment the release ships, not only once the
    // next one starts pruning.
    expect(existsSync(path.join(site, 'scamper-embed-4.8.0.js'))).toBe(true)
    expect(readFileSync(path.join(site, 'scamper-embed-4.8.0.js'), 'utf-8')).toBe(
      'bundle 4.8.0',
    )
    // And the release under review is browsable, so its directory is whole.
    expect(existsSync(path.join(site, '4.8.0/index.html'))).toBe(true)
    expect(existsSync(path.join(site, '4.8.0/assets/scamper-ide-x.js'))).toBe(true)
  })

  test('a candidate does not litter the root', () => {
    const site = compose({}, '4.8.0-rc.1', '4.8.0')

    // It is deleted when the release ships, so a root-level copy would be a
    // file nothing ever cleans up.
    expect(existsSync(path.join(site, 'scamper-embed-4.8.0-rc.1.js'))).toBe(false)
    expect(existsSync(path.join(site, '4.8.0-rc.1/index.html'))).toBe(true)
  })

  test('candidates for the release under review stay side by side', () => {
    const site = compose(
      { '4.8.0-rc.1/index.html': 'older candidate' },
      '4.8.0-rc.2',
      '4.8.0',
    )

    expect(existsSync(path.join(site, '4.8.0-rc.1/index.html'))).toBe(true)
    expect(existsSync(path.join(site, '4.8.0-rc.2/index.html'))).toBe(true)
  })

  test('a candidate for a release that has since shipped is removed', () => {
    const site = compose({ '4.7.0-rc.1/index.html': 'stale' }, '4.8.0', '4.8.0')

    expect(existsSync(path.join(site, '4.7.0-rc.1'))).toBe(false)
  })

  test('a shipped release is lifted to the root and its directory deleted', () => {
    const site = compose(
      {
        '4.7.5/scamper-embed-4.7.5.js': 'bundle 4.7.5',
        '4.7.5/index.html': 'page',
        '4.7.5/assets/chunk.js': 'chunk',
      },
      '4.8.0',
      '4.8.0',
    )

    // This is the flattening: nothing inside needs to survive, so the whole
    // directory goes.
    expect(existsSync(path.join(site, '4.7.5'))).toBe(false)
    expect(readFileSync(path.join(site, 'scamper-embed-4.7.5.js'), 'utf-8')).toBe(
      'bundle 4.7.5',
    )
  })

  test('a release that shipped the unversioned name is copied out, and keeps its directory', () => {
    const site = compose(
      {
        '4.6.0/scamper-embed.js': 'bundle 4.6.0',
        '4.6.0/index.html': 'page',
        '4.6.0/assets/chunk.js': 'chunk',
      },
      '4.8.0',
      '4.8.0',
    )

    // The backfill: it gains a flat URL...
    expect(readFileSync(path.join(site, 'scamper-embed-4.6.0.js'), 'utf-8')).toBe(
      'bundle 4.6.0',
    )
    // ...without losing the one a reading may already be pinned to. Copied, not
    // moved, which is the whole point of this case.
    expect(readFileSync(path.join(site, '4.6.0/scamper-embed.js'), 'utf-8')).toBe(
      'bundle 4.6.0',
    )
    // Reduced to that one file, as before.
    expect(existsSync(path.join(site, '4.6.0/index.html'))).toBe(false)
    expect(existsSync(path.join(site, '4.6.0/assets'))).toBe(false)
  })

  test('an already-backfilled release is not copied over a second time', () => {
    const site = compose(
      {
        'scamper-embed-4.6.0.js': 'the one already at the root',
        '4.6.0/scamper-embed.js': 'bundle 4.6.0',
      },
      '4.8.0',
      '4.8.0',
    )

    expect(readFileSync(path.join(site, 'scamper-embed-4.6.0.js'), 'utf-8')).toBe(
      'the one already at the root',
    )
  })

  test('a directory that shipped no bundle at all is removed', () => {
    const site = compose({ '4.4.0/index.html': 'predates the bundle' }, '4.8.0', '4.8.0')

    expect(existsSync(path.join(site, '4.4.0'))).toBe(false)
  })

  test('the listing names the release under review and every retained bundle', () => {
    const site = compose(
      {
        'scamper-embed-4.5.0.js': 'bundle 4.5.0',
        '4.6.0/scamper-embed.js': 'bundle 4.6.0',
        '4.7.5/scamper-embed-4.7.5.js': 'bundle 4.7.5',
        '4.8.0-rc.1/index.html': 'candidate',
      },
      '4.8.0-rc.2',
      '4.8.0',
    )
    const index = readFileSync(path.join(site, 'index.html'), 'utf-8')

    // The release under review, and its earlier candidate, are browsable.
    expect(index).toContain('href="4.8.0-rc.2/"')
    expect(index).toContain('href="4.8.0-rc.1/"')
    // A shipped release is linked as the file it still has, flat.
    expect(index).toContain('href="scamper-embed-4.7.5.js"')
    expect(index).toContain('href="scamper-embed-4.6.0.js"')
    expect(index).toContain('href="scamper-embed-4.5.0.js"')
    // Pages would otherwise treat this as a Jekyll site.
    expect(existsSync(path.join(site, '.nojekyll'))).toBe(true)
  })

  test('nothing outside the site root is touched', () => {
    const site = compose({ '4.6.0/scamper-embed.js': 'bundle' }, '4.8.0', '4.8.0')

    // The dist it copied from is a sibling; a stray `rm -rf` with an unset
    // variable would take it.
    expect(existsSync(path.join(site, '..', 'dist', 'scamper-embed-4.8.0.js'))).toBe(
      true,
    )
  })
})
