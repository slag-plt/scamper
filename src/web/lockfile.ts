import { OPFSFileSystem } from './fs.js'

export const lockfileName = '.scamper.lock'

export async function acquireLockFile (fs: OPFSFileSystem): Promise<boolean> {
  if (await fs.fileExists(lockfileName)) {
    const origLockTime = new Date(await fs.loadFile(lockfileName))
    const proceed = confirm(`Another Scamper instance may be running (lockfile created at ${origLockTime.toLocaleString()}). Running multiple instances of Scamper may cause data corruption. Click OK to override the lock and continue, or Cancel to abort.`)
    if (!proceed) { return false }
  }
  await fs.saveFile(lockfileName, new Date().toISOString())
  return true
}

export async function releaseLockFile (fs: OPFSFileSystem) {
  if (await fs.fileExists(lockfileName)) {
    await fs.deleteFile(lockfileName)
  }
}