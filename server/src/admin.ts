// The administrator's side of accounts. There is no self-service sign-up and no
// mail, so this is how anyone gets in:
//
//   npm run account -- create ada@example.edu "Ada Lovelace"
//   npm run account -- reset ada@example.edu
//   npm run account -- list
//   npm run account -- delete ada@example.edu
//
// Against the compose stack, the same commands run inside the container:
//
//   docker compose exec server node_modules/.bin/tsx server/src/admin.ts list
//
// It talks to the database directly rather than to the running server, so it
// needs no privileged HTTP route -- there is no way to make an account over the
// network at all, which is the point.

import { createAuth } from './auth'
import { connect } from './db'
import { authBaseUrl, authSecret, databaseUrl } from './env'
import {
  createAccount,
  deleteAccount,
  describeAccount,
  listAccounts,
  renameAccount,
  resetPassword,
} from './accounts'

const USAGE = `Usage:
  account create <email> <name> [password]   make an account
  account reset  <email> [password]          set a new password, ending sessions
  account rename <email> <name>              change the name shown for an account
  account list                               every account
  account info   <email>                     everything held about one account
  account delete <email>                     remove an account and its files

\`chpwd\` is accepted for \`reset\`, matching scripts/server/user-chpwd.

A password is generated when not given. It is shown once and cannot be read
back afterwards -- only replaced -- so pass it on before closing the terminal.`

/** Prints the credential an administrator has to hand over, set apart. */
function announce(email: string, password: string): void {
  console.log('')
  console.log(`  email:    ${email}`)
  console.log(`  password: ${password}`)
  console.log('')
  console.log('Give these to their owner directly. This is the only time the')
  console.log('password is shown.')
}

async function main(): Promise<number> {
  // Typed as possibly-absent because they are: `noUncheckedIndexedAccess` is
  // off, so without this TypeScript reads every argument as a definite string
  // and calls the checks below dead code.
  const argv: (string | undefined)[] = process.argv.slice(2)
  const [command, ...args] = argv

  if (command === undefined || command === 'help' || command === '--help') {
    console.log(USAGE)
    return command === undefined ? 1 : 0
  }

  // Configuration is read before anything else so a missing variable is a
  // sentence rather than a stack trace -- and so the answer, which is usually
  // "you meant to run this against the container", is on screen with it.
  let db
  let auth
  try {
    db = connect(databaseUrl())
    auth = createAuth(db.pool, authSecret(), authBaseUrl())
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error))
    console.error(
      '\nRunning against the compose stack? The database is not reachable ' +
        'from\nthe host by design. Use the scripts, which go through the ' +
        'container:\n  scripts/server/user-list',
    )
    return 1
  }

  try {
    switch (command) {
      case 'create': {
        const [email, name, password] = args
        if (email === undefined || name === undefined) {
          console.error('create needs an email address and a name.')
          return 1
        }
        const created = await createAccount(auth, email, name, password)
        announce(created.email, created.password)
        return 0
      }

      case 'reset':
      case 'chpwd': {
        const [email, password] = args
        if (email === undefined) {
          console.error('reset needs an email address.')
          return 1
        }
        const reset = await resetPassword(auth, email, password)
        console.log(`Signed out every session for ${reset.email}.`)
        announce(reset.email, reset.password)
        return 0
      }

      case 'rename': {
        const [email, name] = args
        if (email === undefined || name === undefined) {
          console.error('rename needs an email address and a new name.')
          return 1
        }
        const renamed = await renameAccount(auth, email, name)
        console.log(`${renamed.email} is now shown as "${renamed.name}".`)
        return 0
      }

      case 'info': {
        const [email] = args
        if (email === undefined) {
          console.error('info needs an email address.')
          return 1
        }
        const account = await describeAccount(auth, db.sql, email)
        if (account === null) {
          console.error(`There is no account for ${email}.`)
          return 1
        }
        console.log(`  email:      ${account.email}`)
        console.log(`  name:       ${account.name}`)
        console.log(`  id:         ${account.id}`)
        console.log(`  verified:   ${account.emailVerified ? 'yes' : 'no'}`)
        console.log(`  created:    ${account.createdAt.toISOString()}`)
        console.log(`  updated:    ${account.updatedAt.toISOString()}`)
        console.log(`  signed in:  ${account.sessions.toString()} session(s)`)
        console.log(`  files:      ${account.files.toString()}`)
        console.log(
          `  history:    ${account.snapshots.toString()} version(s) across ` +
            `${account.histories.toString()} file(s)`,
        )
        // Said plainly rather than omitted: an administrator looking for a
        // password here should leave knowing why there is none to find.
        console.log('  password:   stored as a hash, so it cannot be shown.')
        console.log('              `account chpwd` sets a new one.')
        return 0
      }

      case 'list': {
        const accounts = await listAccounts(auth)
        if (accounts.length === 0) {
          console.log('No accounts yet. Make one with `account create`.')
          return 0
        }
        for (const account of accounts) {
          console.log(
            `${account.email}\t${account.name}\t${account.createdAt.toISOString()}`,
          )
        }
        return 0
      }

      case 'delete': {
        const [email] = args
        if (email === undefined) {
          console.error('delete needs an email address.')
          return 1
        }
        // Their files go too -- `files` and `histories` cascade from `user`.
        const removed = await deleteAccount(auth, email)
        console.log(
          removed
            ? `Removed ${email}, along with their files and history.`
            : `There is no account for ${email}.`,
        )
        return removed ? 0 : 1
      }

      default:
        console.error(`Unknown command: ${command}\n`)
        console.error(USAGE)
        return 1
    }
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error))
    return 1
  } finally {
    db.pool.end()
  }
}

process.exitCode = await main()
