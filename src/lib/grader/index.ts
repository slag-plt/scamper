import * as L from '../../lpm'
import { checkContract, contract } from '../contract.js'
import * as C from '../contract.js'

const Grader: L.Module = new L.Module()

export type Result = Ok | ErrExp | ErrExn | ErrGen
export interface Ok extends L.Struct { [L.structKind]: 'ok', desc: string }
export interface ErrExp extends L.Struct { [L.structKind]: 'exp', desc: string, expected: L.Value, actual: L.Value }
export interface ErrExn extends L.Struct { [L.structKind]: 'exn', desc: string, exn: L.Value }
export interface ErrGen extends L.Struct { [L.structKind]: 'gen', desc: string, reason: string }

/**
 * Currently just a duplicate of the testing library.
 * Desired features / functions to add:
 * Function to prepare stdout 'preamble' (?)
 * Function to print out a specific test with field for the following:
 *   Score if correct (?)
 *   Get feedback on others
 * Function to wrap up stdout (?)
*/

function gradeResultOk (desc: string): Ok {
  checkContract(arguments, contract('grade-result-ok', [C.string]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'ok', desc }
}
Grader.registerValue('grade-result-ok', gradeResultOk)

function gradeResultErrorExpected (desc: string, expected: L.Value, actual: L.Value): ErrExp {
  checkContract(arguments, contract('grade-result-error-expected', [C.string, C.any, C.any]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exp', desc, expected, actual }
}
Grader.registerValue('test-result-error-expected', gradeResultErrorExpected)

function gradeResultErrorExn (desc: string, exn: L.Value): ErrExn {
  checkContract(arguments, contract('grade-result-error-exn', [C.string, C.any]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'exn', desc, exn }
}
Grader.registerValue('grade-result-error-exn', gradeResultErrorExn)

function gradeResultErrorGeneric(desc: string, reason: string): ErrGen {
  checkContract(arguments, contract('grade-result-error-generic', [C.string, C.html]))
  return { [L.scamperTag]: 'struct', [L.structKind]: 'gen', desc, reason }
}
Grader.registerValue('grade-result-error-gen', gradeResultErrorGeneric)

function gradeCase (desc: string, eqFn: L.ScamperFn, expected: L.Value, testFn: L.ScamperFn, score: number, final: boolean): Result {
  checkContract(arguments, contract('grade-case', [C.string, C.func, C.any, C.func]))
  try {
    const actual = L.callScamperFn(testFn)
    const isEqual = L.callScamperFn(eqFn, expected, actual) 
    if (isEqual === true) {
        process.stdout.write("{\n\"score\":")
        process.stdout.write(String(score))
        process.stdout.write("\n}")
        if(!final){
            process.stdout.write(",")
        }
        return gradeResultOk(desc)
    } else if (isEqual === false) {
        process.stdout.write("{\n\"score\":0\n}")
        if(!final){
            process.stdout.write(",")
        }
        return gradeResultErrorExpected(desc, expected, actual)
    } else {
        throw new L.ScamperError('Runtime', `Test case function should have produced a boolean, produced ${L.typeOf(actual)} instead`)
    }
  } catch (e) {
    process.stdout.write("{\n\"score\":0\n}")
    if(!final){
        process.stdout.write(",")
    }
    return gradeResultErrorExn(desc, e as L.Value)
  }
  
}
Grader.registerValue('grade-case', gradeCase)

function gradeExn (desc: string, testFn: L.ScamperFn): Result {
  try {
    L.callScamperFn(testFn, [])
    return gradeResultErrorGeneric(desc, 'Test case did not throw an exception')
  } catch (e) {
    return gradeResultOk(desc)
  }
}
Grader.registerValue('grade-exn', gradeExn)

function gradeOpen() {
    process.stdout.write("{\n\"tests\":\n[")
}
Grader.registerValue('grade-open', gradeOpen)

function gradeClose() {
    process.stdout.write("\n]\n}")
}
Grader.registerValue('grade-close', gradeClose)

function gradeTest(result: Result, score: number, final: boolean) {
    if(isResult(result)){
        if(L.isStructKind(result, 'ok')){
            process.stdout.write("{\n\"score\":")
            process.stdout.write(String(score))
            process.stdout.write("\n}")
        } else {
            process.stdout.write("{\n\"score\":0\n}")
        }
        if(!final){
            process.stdout.write(",")
        }
    }
}
Grader.registerValue('grade-test', gradeTest)

export function isResult (v: any): boolean {
  return L.isStructKind(v, 'ok') || L.isStructKind(v, 'exp')
      || L.isStructKind(v, 'exn') || L.isStructKind(v, 'gen')
}

export default Grader