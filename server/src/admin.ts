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
  listAccounts,
  resetPassword,
} from './accounts'

const USAGE = `Usage:
  account create <email> <name> [password]   make an account
  account reset  <email> [password]          set a new password, ending sessions
  account list                               every account
  account delete <email>                     remove an account and its files

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

  const db = connect(databaseUrl())
  const auth = createAuth(db.pool, authSecret(), authBaseUrl())

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

      case 'reset': {
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
