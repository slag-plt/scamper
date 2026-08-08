// The concrete BetterAuth instance, for the CLI that migrates its tables:
//
//   npm run db:migrate --workspace @scamper/server
//
// The running server builds its own from `createAuth` instead, so this file is
// only ever loaded by the CLI. It exists because the CLI needs an `auth` value
// it can import, whereas the server needs one built from the pool it already
// holds.
import { createAuth } from './auth'
import { connect } from './db'
import { authBaseUrl, authSecret, databaseUrl } from './env'

export const auth = createAuth(
  connect(databaseUrl()).pool,
  authSecret(),
  authBaseUrl(),
)
