#!/usr/bin/env node

/**
 * Runs the front end and the back end together, wired to each other, so the
 * server-backed file system can be exercised locally (issue #357).
 *
 * Equivalent to running these in two terminals:
 *
 *   npm run dev:server
 *   npm run dev -- --mode server
 *
 * `--mode server` is what makes the dev server proxy `/api` to the back end and
 * serve a `/config.json` pointing at it, so the browser talks to one origin --
 * the same shape as production, where the static site and the API share a host.
 *
 * `SCAMPER_SERVER_PORT` moves the back end (and the proxy) off port 3000.
 */

import { spawn } from 'node:child_process'

const npm = process.platform === 'win32' ? 'npm.cmd' : 'npm'
const port = process.env.SCAMPER_SERVER_PORT ?? '3000'

/** @type {import('node:child_process').ChildProcess[]} */
const children = []
let shuttingDown = false

/**
 * Stops both halves, once.
 * @param {number} code the exit status to leave behind
 */
function shutdown(code) {
  if (shuttingDown) return
  shuttingDown = true
  process.exitCode = code
  for (const child of children) {
    if (child.exitCode === null && child.signalCode === null) {
      child.kill('SIGTERM')
    }
  }
}

/**
 * Starts one half, taking the other down with it if it stops.
 * @param {string} name how to refer to this half in a message
 * @param {string[]} args the arguments to `npm`
 * @param {Record<string, string>} env additions to the environment
 */
function run(name, args, env) {
  const child = spawn(npm, args, {
    stdio: 'inherit',
    env: { ...process.env, ...env },
  })
  child.on('error', (error) => {
    console.error(`[dev:full] could not start ${name}: ${error.message}`)
    shutdown(1)
  })
  child.on('exit', (code, signal) => {
    if (shuttingDown) return
    console.error(
      `[dev:full] ${name} exited (${String(signal ?? code)}); stopping the other half.`,
    )
    shutdown(code ?? 1)
  })
  children.push(child)
}

// Ctrl-C reaches both children directly (they share this process group), but
// handle it anyway so a signal sent to this process alone still cleans up.
process.on('SIGINT', () => {
  shutdown(0)
})
process.on('SIGTERM', () => {
  shutdown(0)
})

console.info(
  `[dev:full] back end on :${port}, front end proxying /api to it.\n` +
    '[dev:full] The IDE will use the server instead of local storage.',
)

run('the back end', ['run', 'dev:server'], { PORT: port })
run('the front end', ['run', 'dev', '--', '--mode', 'server'], {
  SCAMPER_SERVER_PORT: port,
})
