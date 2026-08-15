// Accounts, made by hand.
//
// There is no mail server and no self-service sign-up: an administrator creates
// each account and passes the password to the person out of band, and a
// forgotten password is another visit to the administrator. That is a
// deliberate constraint, not a gap waiting to be filled -- every route that
// would normally send mail (verification, reset links) needs a mail transport
// this deployment does not have, and a reset link is only as trustworthy as the
// mailbox it lands in.
//
// These use BetterAuth's own internals rather than its HTTP routes, because the
// public sign-up route is turned off (`disableSignUp`). Going through
// `internalAdapter` and `password.hash` means the rows written here are exactly
// the rows a sign-up would have written -- same hashing, same shape -- so
// signing in cannot tell the difference.

import { randomInt } from 'node:crypto'

import type { RowDataPacket } from 'mysql2'
import type { Pool as SqlPool } from 'mysql2/promise'

import type { Auth } from './auth'

/** BetterAuth's name for an email-and-password account. */
const CREDENTIAL_PROVIDER = 'credential'

/**
 * The alphabet generated passwords are drawn from.
 *
 * No `0`/`O`, `1`/`l`/`I`, `5`/`S`, or `8`/`B`: these get read aloud, written
 * on paper, and typed by someone who did not choose them, and a password that
 * cannot be transcribed is a support request.
 */
const ALPHABET = 'abcdefghijkmnpqrstuvwxyzACDEFGHJKLMNPQRTUVWXYZ2346799'

/** How many characters a generated password gets. */
const PASSWORD_LENGTH = 16

/** What an administrator sees about an account. */
export interface AccountSummary {
  email: string
  name: string
  createdAt: Date
}

/**
 * Everything an administrator can learn about one account.
 *
 * Everything *except* the password, which is not stored and so cannot be shown:
 * `account` holds a hash, and a hash is not reversible by design. The honest
 * answer to "what is their password" is to set a new one and hand it over --
 * `resetPassword`, which is what the `chpwd` script does.
 */
export interface AccountDetail extends AccountSummary {
  id: string
  emailVerified: boolean
  updatedAt: Date
  /** How many sessions are live, i.e. on how many machines they are signed in. */
  sessions: number
  files: number
  /** Files with a recorded history, including deleted ones still recoverable. */
  histories: number
  snapshots: number
}

/**
 * @returns a password suitable for handing to someone
 *
 * `randomInt` rather than `Math.random`, and rejection-free because the
 * alphabet's length divides the range it draws from evenly enough for
 * `randomInt` to handle the bias itself. Sixteen characters of this alphabet is
 * about 91 bits, which is far past anything worth guessing.
 */
export function generatePassword(): string {
  let password = ''
  for (let i = 0; i < PASSWORD_LENGTH; i++) {
    password += ALPHABET[randomInt(ALPHABET.length)]
  }
  return password
}

/**
 * Creates an account and returns the password to give its owner.
 *
 * The address is marked verified because the administrator is the one
 * vouching for it -- there is no mail to confirm it with, and leaving it
 * unverified would only mean a flag nothing ever clears.
 *
 * @param password the password to set, or a generated one if omitted
 * @throws if an account with this address already exists
 */
export async function createAccount(
  auth: Auth,
  email: string,
  name: string,
  password = generatePassword(),
): Promise<{ email: string; password: string }> {
  const ctx = await auth.$context

  const existing = await ctx.internalAdapter.findUserByEmail(email)
  if (existing !== null) {
    throw new Error(`There is already an account for ${email}.`)
  }

  const user = await ctx.internalAdapter.createUser({
    email,
    name,
    emailVerified: true,
  })
  await ctx.internalAdapter.createAccount({
    userId: user.id,
    providerId: CREDENTIAL_PROVIDER,
    accountId: user.id,
    password: await ctx.password.hash(password),
  })

  return { email, password }
}

/**
 * Sets a new password for an existing account, and signs it out everywhere.
 *
 * Ending the sessions matters: the reason to reset a password is usually that
 * someone else might know the old one, and leaving live sessions alone would
 * keep whoever it was signed in.
 *
 * @param password the password to set, or a generated one if omitted
 * @throws if there is no account with this address
 */
export async function resetPassword(
  auth: Auth,
  email: string,
  password = generatePassword(),
): Promise<{ email: string; password: string }> {
  const ctx = await auth.$context

  const found = await ctx.internalAdapter.findUserByEmail(email)
  if (found === null) {
    throw new Error(`There is no account for ${email}.`)
  }

  await ctx.internalAdapter.updatePassword(
    found.user.id,
    await ctx.password.hash(password),
  )
  await ctx.internalAdapter.deleteUserSessions(found.user.id)

  return { email, password }
}

/**
 * Changes the name shown for an account. The address is the identifier and is
 * left alone: it is what they sign in with, and what every file they own is
 * keyed to through `user.id`.
 *
 * @throws if there is no account with this address
 */
export async function renameAccount(
  auth: Auth,
  email: string,
  name: string,
): Promise<AccountSummary> {
  const ctx = await auth.$context

  const found = await ctx.internalAdapter.findUserByEmail(email)
  if (found === null) {
    throw new Error(`There is no account for ${email}.`)
  }

  await ctx.internalAdapter.updateUser(found.user.id, { name })
  return { email, name, createdAt: found.user.createdAt }
}

/**
 * Everything held about one account, for an administrator answering "what is
 * going on with this student".
 *
 * The counts come from our own tables rather than BetterAuth's, which is why
 * this needs the pool as well: how many files someone has is usually the
 * question behind the question.
 *
 * @returns the account, or null if there is none with this address
 */
export async function describeAccount(
  auth: Auth,
  sql: SqlPool,
  email: string,
): Promise<AccountDetail | null> {
  const ctx = await auth.$context

  const found = await ctx.internalAdapter.findUserByEmail(email)
  if (found === null) return null
  const { user } = found

  const sessions = await ctx.internalAdapter.listSessions(user.id)
  const [rows] = await sql.query<RowDataPacket[]>(
    `SELECT
       (SELECT COUNT(*) FROM files WHERE user_id = ?)      AS files,
       (SELECT COUNT(*) FROM histories WHERE user_id = ?)  AS histories,
       (SELECT COUNT(*) FROM snapshots s
          JOIN histories h ON h.id = s.history_id
         WHERE h.user_id = ?)                              AS snapshots`,
    [user.id, user.id, user.id],
  )

  return {
    id: user.id,
    email: user.email,
    name: user.name,
    emailVerified: user.emailVerified,
    createdAt: user.createdAt,
    updatedAt: user.updatedAt,
    sessions: sessions.length,
    files: Number(rows[0].files),
    histories: Number(rows[0].histories),
    snapshots: Number(rows[0].snapshots),
  }
}

/** @returns every account, oldest first */
export async function listAccounts(auth: Auth): Promise<AccountSummary[]> {
  const ctx = await auth.$context
  const users = await ctx.internalAdapter.listUsers()

  return users
    .map((user) => ({
      email: user.email,
      name: user.name,
      createdAt: user.createdAt,
    }))
    .sort((a, b) => a.createdAt.getTime() - b.createdAt.getTime())
}

/**
 * Removes an account, and with it the person's files: `files` and `histories`
 * are `ON DELETE CASCADE` from `user` (see schema.sql), so this is not
 * recoverable.
 *
 * @returns true iff there was an account to remove
 */
export async function deleteAccount(
  auth: Auth,
  email: string,
): Promise<boolean> {
  const ctx = await auth.$context

  const found = await ctx.internalAdapter.findUserByEmail(email)
  if (found === null) return false

  await ctx.internalAdapter.deleteUser(found.user.id)
  return true
}
