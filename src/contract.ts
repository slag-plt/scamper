import { ScamperError, ICE } from './lang.js'
import { typeOfValue } from './value.js'
import * as V from './value.js'

export interface Spec {
  predicate: (v: any) => boolean
  errorMsg: (actual: any) => string
}

export const any: Spec = {
  predicate: (v: any) => true,
  errorMsg: (actual: any) => { throw new ICE('anyC.errorMsg', 'anyC should not produce an error!') }
}

export const and = (...specs: Spec[]): Spec => ({
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

export const or = (...specs: Spec[]): Spec => ({
  predicate: (v: any) => specs.some((s) => s.predicate(v)),
  errorMsg: (actual: any) => {
    const msgs = specs.map((s) => s.errorMsg(actual))
    return `expected one of:\n  ${msgs.join('\n  ')}`
  }
})

export const boolean = {
  predicate: (v: any) => typeof v === 'boolean',
  errorMsg: (actual: any) => `expected a boolean, received ${typeOfValue(actual)}`
} 

export const number = {
  predicate: (v: any) => typeof v === 'number',
  errorMsg: (actual: any) => `expected a number, received ${typeOfValue(actual)}`
}

export const string = {
  predicate: (v: any) => typeof v === 'string',
  errorMsg: (actual: any) => `expected a string, received ${typeOfValue(actual)}`
}

export const integer = {
  predicate: (v: any) => typeof v === 'number' && Math.floor(v) === v,
  errorMsg: (actual: any) => `expected an integer, received ${typeOfValue(actual)}`
}

export const nat = {
  predicate: (v: any) => typeof v === 'number' && Math.floor(v) === v && v >= 0,
  errorMsg: (actual: any) => `expected a natural number, received ${typeOfValue(actual)}`
}

export const func = {
  predicate: (v: any) => V.isFunction(v),
  errorMsg: (actual: any) => `expected a function, received ${typeOfValue(actual)}`
}

export const char = {
  predicate: (v: any) => V.isChar(v),
  errorMsg: (actual: any) => `expected a character, received ${typeOfValue(actual)}`
}

export const pair = {
  predicate: (v: any) => V.isPair(v),
  errorMsg: (actual: any) => `expected a pair, received ${typeOfValue(actual)}`
}

export const list = {
  predicate: (v: any) => V.isList(v),
  errorMsg: (actual: any) => `expected a list, received ${typeOfValue(actual)}`
}

export const vector = {
  predicate: (v: any) => Array.isArray(v),
  errorMsg: (actual: any) => `expected a vector, received ${typeOfValue(actual)}`
}

export const equal = (expected: any) => ({
  predicate: (v: any) => V.valuesEqual(expected, v),
  errorMsg: (actual: any) => `expected ${expected}, received ${actual}`
})

export type Contract = { funcName: string, params: Spec[], varargs?: Spec }
export const contract = (funcName: string, params: Spec[], varargs?: Spec): Contract => ({ funcName, params, varargs })

export function checkContract (contract: Contract, args: any[]): void {
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