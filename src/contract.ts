import { ScamperError, ICE } from './lang.js'

interface Spec {
  predicate: (v: any) => boolean
  errorMsg: (actual: any) => string
}

const anyC: Spec = {
  predicate: (v: any) => true,
  errorMsg: (actual: any) => { throw new ICE('anyC.errorMsg', 'anyC should not produce an error!') }
}

const andC = (...specs: Spec[]): Spec => ({
  predicate: (v: any) => specs.every((s) => s.predicate(v)),
  errorMsg: (actual: any) => {
    for (const spec of specs) {
      if (!spec.predicate(actual)) {
        return spec.errorMsg(actual)
      }
    }
    throw new ICE('andC.errorMsg', 'andC should have found a failing spec!')
  }
})

type Contract = { funcName: string, params: Spec[], varargs?: Spec }

function checkContract (contract: Contract, args: any[]): void {
  if (contract.varargs === undefined && args.length !== contract.params.length) {
    throw new ScamperError('Runtime', `wrong number of arguments to ${contract.funcName}. Expected ${contract.params.length}, received ${args.length}.`)
  }
  if (contract.varargs !== undefined && args.length < contract.params.length) {
    throw new ScamperError('Runtime', `wrong number of arguments to ${contract.funcName}. Expected at least ${contract.params.length}, received ${args.length}.`)
  }
  const required   = args.slice(0, contract.params.length)
  const additional = args.slice(contract.params.length)
  required.forEach((arg, i) => {
    if (!contract.params[i].predicate(arg)) {
      throw new ScamperError('Runtime', contract.params[i].errorMsg(arg))
    }
  })
  if (contract.varargs !== undefined) {
    additional.forEach((arg) => {
      if (!contract.varargs!.predicate(arg)) {
        throw new ScamperError('Runtime', contract.varargs!.errorMsg(arg))
      }
    })
  }
}