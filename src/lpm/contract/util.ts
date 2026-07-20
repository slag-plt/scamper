import * as L from "../lang"
import * as U from "../util"

/**
 * Produces a short, human-readable description of a predicate's name suitable
 * for embedding in an "expected ..." contract violation message, mirroring
 * the phrasing used by the specs in src/js/contract.ts, e.g., `number?` ~>
 * `a number`, `integer?` ~> `an integer`.
 */
function describePred(pred: string): string {
  const name = pred.endsWith("?") ? pred.slice(0, -1) : pred
  const article = /^[aeiou]/i.test(name) ? "an" : "a"
  return `${article} ${name}`
}

/**
 * Creates a block of code that computes a contract violation message purely
 * via prelude/runtime calls (as opposed to a host closure), so that the
 * result is ordinary Scamper bytecode rather than something tied to this
 * module's scope. Evaluates to:
 *
 *   (string-append "expected " descPred ", received " (typeOf argVar))
 *
 * @param descPred the (already-rendered) description of the expected value,
 *                  e.g., `describePred("number?")` ~> `"a number"`
 * @param argVar the name of the (in-scope) variable holding the offending
 *               argument
 * @return a `Blk` that leaves the rendered message string on top of the stack
 */
function createErrorMsgBlk(descPred: string, argVar: string): L.Blk {
  return [
    U.mkVar("string-append"),
    U.mkLit("expected "),
    U.mkLit(descPred),
    U.mkLit(", received "),
    U.mkVar("type-of"),
    U.mkVar(argVar),
    U.mkAp(1),
    U.mkAp(4),
  ]
}

/**
 * Creates a block of code consisting of a single closure literal, one
 * parameter per entry of `preds`, that checks each argument against its
 * corresponding predicate in a cascading if-chain before invoking `fn` with
 * all of the arguments, e.g., for `createCheckBlk("f", ["number?", "string?"])`:
 *
 *   (lambda (x0 x1)
 *     (if (number? x0)
 *         (if (string? x1)
 *             (f x0 x1)
 *             (error "expected a string, received ..."))
 *         (error "expected a number, received ...")))
 *
 * @param fn the name of the function to invoke once every check succeeds
 * @param preds the name of the predicate function used to check the argument
 *              at the corresponding position
 * @return a `Blk` containing the single closure literal implementing the checks
 */
export function createCheckBlk(fn: string, preds: string[]): L.Blk {
  const params = Array.from(
    { length: preds.length },
    (_, i) => `##arg${i.toString()}##`,
  )

  const checkArgAt = (i: number): L.Blk => {
    if (i === preds.length) {
      return [U.mkVar(fn), ...params.map((p) => U.mkVar(p)), U.mkAp(params.length)]
    }
    const pred = preds[i]
    const argVar = params[i]
    return [
      U.mkVar(pred),
      U.mkVar(argVar),
      U.mkAp(1),
      U.mkMatch([
        [U.mkPLit(true), checkArgAt(i + 1)],
        [
          U.mkPLit(false),
          [
            U.mkVar("error"),
            ...createErrorMsgBlk(describePred(pred), argVar),
            U.mkAp(1),
          ],
        ],
      ]),
    ]
  }

  return [U.mkCls(params, checkArgAt(0), fn)]
}
