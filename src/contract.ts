import { ScamperError, ICE, Value } from './lang.js'

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
  errorMsg: (actual: any) => `expected a boolean, received ${Value.typeOf(actual)}`
} 

export const number = {
  predicate: (v: any) => typeof v === 'number',
  errorMsg: (actual: any) => `expected a number, received ${Value.typeOf(actual)}`
}

export const string = {
  predicate: (v: any) => typeof v === 'string',
  errorMsg: (actual: any) => `expected a string, received ${Value.typeOf(actual)}`
}

export const numRange = (min: number, max: number) => ({
  predicate: (v: any) => typeof v === 'number' && v >= min && v <= max,
  errorMsg: (actual: any) => `expected a number in the range [${min}, ${max}], received ${Value.typeOf(actual)}`
})

export const integer = {
  predicate: (v: any) => typeof v === 'number' && Math.floor(v) === v,
  errorMsg: (actual: any) => `expected an integer, received ${Value.typeOf(actual)}`
}

export const nat = {
  predicate: (v: any) => typeof v === 'number' && Math.floor(v) === v && v >= 0,
  errorMsg: (actual: any) => `expected a natural number, received ${Value.typeOf(actual)}`
}

export const pos = {
  predicate: (v: any) => typeof v === 'number' && v > 0,
  errorMsg: (actual: any) => `expected a positive number, received ${Value.typeOf(actual)}`
}

export const nonneg = {
  predicate: (v: any) => typeof v === 'number' && v >= 0,
  errorMsg: (actual: any) => `expected a non-negative number, received ${Value.typeOf(actual)}`
}

export const func = {
  predicate: (v: any) => Value.isFunction(v),
  errorMsg: (actual: any) => `expected a function, received ${Value.typeOf(actual)}`
}

export const char = {
  predicate: (v: any) => Value.isChar(v),
  errorMsg: (actual: any) => `expected a character, received ${Value.typeOf(actual)}`
}

export const pair = {
  predicate: (v: any) => Value.isPair(v),
  errorMsg: (actual: any) => `expected a pair, received ${Value.typeOf(actual)}`
}

export const list = {
  predicate: (v: any) => Value.isList(v),
  errorMsg: (actual: any) => `expected a list, received ${Value.typeOf(actual)}`
}

export const nonemptyList = {
  predicate: (v: any) => Value.isList(v) && v !== null,
  errorMsg: (actual: any) => `expected a non-empty list, received ${Value.typeOf(actual)}`
}

export const listof = (spec: Spec) => ({
  predicate: (v: any) => {
    if (!Value.isList(v)) {
      return false
    }
    let lst = v
    while (lst !== null) {
      if (!spec.predicate(lst.fst)) {
        return false
      }
      lst = lst.snd
    }
    return true
  },
  errorMsg: (actual: any) => {
    if (!Value.isList(actual)) {
      return `expected a list, received ${Value.typeOf(actual)}`
    } else {
      let lst = actual
      while (lst !== null) {
        if (!spec.predicate(lst.fst)) {
          return spec.errorMsg(lst.fst)
        }
        lst = lst.snd
      }
      throw new ICE('listofC.errorMsg', 'listofC should have found a failing spec!')
    }
  }
})

export const vector = {
  predicate: (v: any) => Array.isArray(v),
  errorMsg: (actual: any) => `expected a vector, received ${Value.typeOf(actual)}`
}

export const struct = (kind: string) => ({
  predicate: (v: any) => Value.isStructKind(v, kind),
  errorMsg: (actual: any) => `expected a struct of kind ${kind}, received ${Value.typeOf(actual)}`
})

export const equal = (expected: any) => ({
  predicate: (v: any) => Value.equal(expected, v),
  errorMsg: (actual: any) => `expected ${expected}, received ${actual}`
})

export type Contract = { funcName: string, params: Spec[], varargs?: Spec }
export const contract = (funcName: string, params: Spec[], varargs?: Spec): Contract => ({ funcName, params, varargs })

export function checkContract (args: IArguments, contract: Contract): void {
  if (contract.varargs === undefined && args.length !== contract.params.length) {
    throw new ScamperError('Runtime', `wrong number of arguments to ${contract.funcName} provided. Expected ${contract.params.length}, received ${args.length}.`)
  }
  if (contract.varargs !== undefined && args.length < contract.params.length) {
    throw new ScamperError('Runtime', `wrong number of arguments to ${contract.funcName} provided. Expected at least ${contract.params.length}, received ${args.length}.`)
  }
  const required = Array.prototype.slice.call(args, 0, contract.params.length)
  const additional = Array.prototype.slice.call(args, contract.params.length, args.length)
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