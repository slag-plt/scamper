/**
 * Raised when the file server refuses a request because there is no session.
 *
 * A distinct type because 401 is not an error in the way the others are: the
 * files are fine, the request was well formed, and the answer is to sign in
 * again rather than to report a failure. A session can lapse mid-edit -- it
 * expires, or is signed out in another tab -- so any request can meet this,
 * and the UI needs to tell it apart from a real fault.
 */
export class NotSignedInError extends Error {
  constructor() {
    super('Your session has ended. Sign in again to reach your files.')
    this.name = 'NotSignedInError'
  }
}

/** @returns true iff `error` is the file server reporting no session. */
export function isNotSignedIn(error: unknown): boolean {
  return error instanceof NotSignedInError
}
