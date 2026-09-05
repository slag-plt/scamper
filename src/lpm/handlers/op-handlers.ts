import { ICE, ScamperError, SetRecursionDepthSignal, SuspendSignal } from '../error'
import { Fiber, minorStep, StepResult, traceStep } from '../fiber'
import { Ops, Scope, Value } from '../lang'
import { Frame } from '../frame'
import { Range } from '../range'
import { isClosure, isJsFunction, isList, listToVector, mkClosure, mkLet, mkMatch, patVars, pMatch, popRequired, typeOf, vectorToList } from '../util'

/* Definition */
type OpHandler<T extends Ops['tag']> = (
  op: Extract<Ops, { tag: T }>,
  currFrame: Frame,
  fiber: Fiber,
) => StepResult

/* Handlers */
export const LitHandler: OpHandler<'lit'> = (op, currFrame) => {
  currFrame.values.push(op.value)
  return minorStep
}

export const VarHandler: OpHandler<'var'> = (op, currFrame) => {
  if (!currFrame.env.has(op.name)) {
    throw new ScamperError('Runtime', `Variable not found: ${op.name}`)
  }
  const value = currFrame.env.get(op.name)
  // Library code naming another library function by its top-level name gets
  // the value *behind* its contract wrapper (#476). The check exists to
  // describe a student's mistake at their own call site; re-running it on each
  // of `map`'s own steps buys nothing and dominates the cost of every library
  // call. A *local* name is left wrapped: it holds whatever the caller passed
  // in, so `(map char-upcase ...)` still checks each character.
  if (
    currFrame.origin === 'builtin' &&
    isClosure(value) &&
    value.contractTarget !== undefined &&
    !currFrame.env.isLocal(op.name)
  ) {
    currFrame.values.push(value.contractTarget)
    return minorStep
  }
  currFrame.values.push(value)
  return minorStep
}

export const ClsHandler: OpHandler<'cls'> = (op, currFrame) => {
  currFrame.values.push(
    mkClosure(
      op.params,
      op.body,
      // Capture the scope stack by reference so a binding filled after this
      // closure is created (letrec) is visible through the shared scopes.
      currFrame.env.getScopes(),
      // TODO: this dummy function should exist until we remove all calls to L.callScamperFn
      () => {
        throw new ICE('Fiber.ClsHandler', 'Closure.call was deprecated!')
      },
      op.name,
      op.restParam,
      // A closure is stamped with the origin of the code that created it (see
      // CodeOrigin) -- the enclosing frame's, so that a lambda a library
      // function builds at *call* time is library code too, exactly as one it
      // defines at load time is. That is what keeps `(list-of number?)`'s
      // result reducing atomically in a trace rather than spilling prelude's
      // `and`/`all-satisfy?` into it, and what lets `vector-map`'s inner loop
      // call `vector-ref` without its contract.
      currFrame.origin,
      // Inherit the enclosing frame's home so a lambda returned by a
      // qualified-module function still resolves the module's siblings.
      currFrame.home,
    ),
  )
  return minorStep
}

/**
 * Shared by ApHandler (a statically-known arg count baked into the "ap" op
 * at compile time) and ApSpreadHandler (ap-spread's arg count is only known at
 * runtime, from the length of the spread list) -- both ultimately need the
 * same dispatch: call directly if fn is a JsFunction (rewriting a thrown
 * error's range to the call site), or push a new Frame if fn is a
 * Closure (mirroring what a "cls" body does on every other call), since
 * Closure.call/callScamperFn are permanently disabled for JS code calling
 * back into Scamper.
 */
export function applyFn(
  fn: Value,
  args: Value[],
  currFrame: Frame,
  fiber: Fiber,
  range: Range,
): StepResult {
  if (isJsFunction(fn)) {
    try {
      currFrame.values.push(fn(...args))
      return traceStep
    } catch (e) {
      if (e instanceof SetRecursionDepthSignal) {
        // `set-maximum-recursion-depth!` reaching the fiber it applies to (the
        // primitive has no handle on one). The depth is already validated, so
        // this is the whole of the call: set it and push the void the docstring
        // promises. The signal is handled here and never escapes.
        fiber.setMaxCallStackDepth(e.depth)
        currFrame.values.push(undefined)
        return traceStep
      }
      // N.B., a synthetic frame name ("##anonymous##", "##stmt-N##") means
      // fn was called directly, outside any named Scamper function -- in
      // that case op.range/fn.name (this call's own, real range and the
      // JS function's own name) are already exactly right. Otherwise fn
      // is being invoked from inside a named function's frame -- most
      // often a contract wrapper's own ##contract-target## call, whose Ap
      // op only ever carries the *wrapped definition's* range, never the
      // user's actual call site. In that case prefer the frame's own
      // callRange/name: the range/name of the Ap that invoked *this
      // frame*, i.e. wherever the user (or an enclosing function) really
      // wrote the call.
      const useFrame = !currFrame.name.startsWith('##')
      const callRange = useFrame ? currFrame.callRange : range
      if (e instanceof SuspendSignal) {
        // A blocking primitive is suspending the fiber -- propagate to
        // Scheduler.stepTask (control flow, not an error). The result value is
        // supplied on resume by Fiber.resumeWithValue, not by the push above.
        // Record the call site on the way out: the action's own error is raised
        // later, in the scheduler, with no other route back here (#342).
        //
        // N.B., only when that site is worth showing. An *unnamed* library
        // frame is a lambda inside library source, so its op ranges point into
        // prelude.scm/image.scm, and underlining a line of the standard library
        // in the student's editor is worse than no range at all -- those still
        // report unlocated. A *named* one (`with-file`,
        // `with-image-from-url`) reports its frame's callRange, which a call
        // made from library code passes along from its own caller (see the
        // Frame built below), so it is the student's call after all.
        if (useFrame || currFrame.origin === 'user') {
          e.range ??= callRange
        }
        throw e
      }
      if (e instanceof ScamperError) {
        // Fill range/source only when the error didn't set them itself: most JS
        // primitives throw context-free errors (we supply both), but some (e.g.
        // `error`) fix their own source, which we must not clobber.
        e.range ??= callRange
        e.source ??= useFrame ? currFrame.name : fn.name
        throw e
      } else {
        throw new ScamperError(
          'Runtime',
          `Unexpected error in Javascript function call: ${e instanceof Error ? e.toString() : String(e)}`,
          undefined,
          callRange,
          undefined
        )
      }
    }
  }
  if (isClosure(fn)) {
    // With rest parameters, we have an arity mismatch if we have fewer arguments than the
    // required number or not the exact number if there is no rest parameter.
    if (args.length < fn.params.length ||
        (!fn.restParam && args.length !== fn.params.length)) {
      throw new ScamperError(
        'Runtime',
        `Arity mismatch in function call: expected ${fn.params.length.toString()} arguments, got ${args.length.toString()}`,
        undefined,
        range,
        undefined)
    }
    const namedArgs = args.slice(0, fn.params.length)
    const bindings = fn.params.map((p: string, i: number): [string, Value] =>
      [p, namedArgs[i]]).concat(
        fn.restParam ? [[fn.restParam, vectorToList(args.slice(fn.params.length))]] : [])
    // The callee sees the closure's captured scopes (shared, so letrec fills
    // are visible) with the parameters as a fresh innermost scope on top. Its
    // free top-level names resolve against the closure's `home` env when set (a
    // qualified-module closure -- see Closure.home), else the running fiber's
    // env (so top-level forward references / letrec keep working).
    const paramScope: Scope = new Map(bindings)
    const newFrame = new Frame(
      fn.name ?? '##anonymous##',
      (fn.home ?? fiber.topLevelEnv).withLocalScopes([...fn.locals, paramScope]),
      fn.code,
      // A call written in library source has no site in the student's program:
      // `range` points into a .scm of the standard library, and underlining
      // that in their editor is worse than no range at all. Such a call passes
      // along the range *its* caller was called from instead, so an error
      // raised deep in the library still points at the call the student wrote
      // -- which matters more now that a library-internal call runs without
      // its contract check and can fail inside the wrapped function (#476).
      currFrame.origin === 'builtin' ? currFrame.callRange : range,
      fn.origin ?? 'user',
      fn.home,
    )
    if (currFrame.canTailCall()) {
      // tail-call optimize by replacing the current frame (any leftover
      // pop-scope ops would only discard scopes we're dropping with the frame)
      fiber.replaceFrame(newFrame)
    } else {
      fiber.pushFrame(newFrame)
    }
    return traceStep
  }
  throw new ScamperError(
    'Runtime',
    `Not a function or closure: ${JSON.stringify(fn)}`,
    undefined,
    range,
    undefined
  )
}

export const ApHandler: OpHandler<'ap'> = (op, currFrame, fiber) => {
  if (currFrame.values.length < op.numArgs + 1) {
    throw new ICE(
      'Fiber.ApHandler',
      `Not enough values for application: expected ${(op.numArgs + 1).toString()}, currently have ${currFrame.values.length.toString()}`,
    )
  }
  const values = currFrame.values.splice(-(op.numArgs + 1))
  const fn = values[0]
  const args = op.numArgs === 0 ? [] : values.splice(-op.numArgs)
  return applyFn(fn, args, currFrame, fiber, op.range)
}

export const ApSpreadHandler: OpHandler<'ap-spread'> = (op, currFrame, fiber) => {
  if (currFrame.values.length < 2) {
    throw new ICE(
      'Fiber.ApSpreadHandler',
      `Not enough values for ap-spread: expected 2, currently have ${currFrame.values.length.toString()}`,
    )
  }
  const [fn, argList] = currFrame.values.splice(-2)
  if (!isList(argList)) {
    throw new ScamperError(
      'Runtime',
      `expected a list, received ${typeOf(argList)}`,
      undefined,
      op.range,
      'apply',
    )
  }
  return applyFn(fn, listToVector(argList), currFrame, fiber, op.range)
}

export const MatchHandler: OpHandler<'match'> = (op, currFrame) => {
  // Check length, not `pop() === undefined`: `void` is a legitimate value that
  // *is* `undefined`, so a scrutinee of `void` (e.g. `(let ([_ (display x)])
  // ...)`) must not be mistaken for an empty stack.
  if (currFrame.values.length === 0) {
    throw new ICE('Fiber.MatchHandler', 'Match requires at least one value')
  }
  const scrutinee = currFrame.values.pop()
  // One branch per step, to keep the work quantum small. The cursor advances
  // by re-entering with a fresh op copy (as `let` does), never by writing to
  // `op`: the compiled program is shared by every run and every fiber.
  const currBranch = op.branches.at(op.idx)
  if (!currBranch) {
    // A `match` in library source has no site in the student's program, so a
    // builtin frame blames the call the student wrote instead of prelude.scm,
    // exactly as the call range at :190 does.
    throw new ScamperError(
      'Runtime',
      'Inexhaustive pattern match failure',
      undefined,
      currFrame.origin === 'builtin' ? currFrame.callRange : op.range,
      undefined
    )
  }
  const [pat, blk] = currBranch
  const bindings = pMatch(scrutinee, pat)
  if (!bindings) {
    currFrame.pushBlk([mkMatch(op.branches, op.range, op.idx + 1)])
    // make sure to push the scrutinee back for the next branch!
    currFrame.values.push(scrutinee)
  } else {
    // Push the branch's binders as a fresh scope; the `pop-scope` codegen
    // emits right after this match op discards it once the branch completes.
    currFrame.env = currFrame.env.pushScope(bindings)
    currFrame.pushBlk(blk)
  }
  return traceStep
}

export const LetHandler: OpHandler<'let'> = (op, currFrame) => {
  // letrec: on the first run (idx 0) declare every binder as a HOLE in one
  // shared scope; on each later run, the previous binding's value is on the
  // stack -- match it and fill its holes. Then evaluate the next binding's
  // value (re-entering with idx+1 via a fresh op copy) or run the body. The
  // trailing `pop-scope` codegen emits discards the scope after the body.
  const k = op.bindings.length
  if (op.idx === 0) {
    currFrame.env = currFrame.env.declareScope(
      op.bindings.flatMap((b) => patVars(b.pat)),
    )
  } else {
    if (currFrame.values.length === 0) {
      throw new ICE('Fiber.LetHandler', 'let binding value missing from the stack')
    }
    const value = currFrame.values.pop()
    const binding = op.bindings[op.idx - 1]
    const bs = pMatch(value, binding.pat)
    if (!bs) {
      // The failing binder's own range, not the whole `let`: `idx` says which
      // binding this is, so the underline can land on the very pattern the
      // message names instead of a form that may span many bindings. A `let`
      // in library source has no site in the student's program, so a builtin
      // frame blames the call they wrote, exactly as :190 and :265 do.
      throw new ScamperError(
        'Runtime',
        binding.failMsg ?? 'let: value did not match its pattern',
        undefined,
        currFrame.origin === 'builtin' ? currFrame.callRange : binding.pat.range,
        undefined
      )
    }
    for (const [name, v] of bs) {
      currFrame.env.assign(name, v)
    }
  }
  if (op.idx < k) {
    currFrame.pushBlk([
      ...op.bindings[op.idx].value,
      mkLet(op.bindings, op.body, op.range, op.idx + 1, op.provenance),
    ])
  } else {
    currFrame.pushBlk(op.body)
  }
  return traceStep
}

export const IfHandler: OpHandler<'if'> = (op, currFrame) => {
  // The guard was evaluated inline and is on top of the value stack.
  if (currFrame.values.length === 0) {
    throw new ICE('Fiber.IfHandler', 'if requires a guard value')
  }
  const guard = currFrame.values.pop()
  if (guard === true) {
    currFrame.pushBlk(op.thenB)
  } else if (guard === false) {
    currFrame.pushBlk(op.elseB)
  } else {
    throw new ScamperError(
      'Runtime',
      `if: expected a boolean guard, received ${typeOf(guard)}`,
    )
  }
  return traceStep
}

export const PopScopeHandler: OpHandler<'pop-scope'> = (_op, currFrame) => {
  currFrame.env = currFrame.env.popScope()
  return minorStep
}

// N.B., installs the exception handler for the guarded application. The stack is
// [.., handler, fn]: the handler is *peeked* (at -2), not popped -- both it and
// fn stay on the value stack (between here and the matching pop-handler) so
// raise.ts can reconstruct the `with-handler` form and the fiber can find the
// handler on the stack. We record the frame depth and the value-stack depth to
// reset to (dropping both handler and fn) if a ScamperError is raised under the
// guarded application (see Fiber.handleError).
export const PushHandlerHandler: OpHandler<'push-handler'> = (_op, currFrame, fiber) => {
  if (currFrame.values.length < 2) {
    throw new ICE('Fiber.PushHandlerHandler', 'Expected a handler and a guarded function on the stack')
  }
  fiber.handlerStack.push({
    frameDepth: fiber.frames.length,
    baseDepth: currFrame.values.length - 2,
    // The length check above is what makes this safe; `at` would only be
    // narrowed by an assertion saying the same thing less clearly.
    handler: currFrame.values[currFrame.values.length - 2],
  })
  return minorStep
}

// N.B., normal (no-error) completion of a guarded application. The stack is
// [.., handler, result]; drop the handler value and its stack record, leaving
// [.., result].
export const PopHandlerHandler: OpHandler<'pop-handler'> = (_op, currFrame, fiber) => {
  if (currFrame.values.length < 2) {
    throw new ICE(
      'Fiber.PopHandlerHandler',
      'Expected a handler and a result on the stack',
    )
  }
  const result = popRequired(currFrame.values, "the frame's value stack")
  currFrame.values.pop() // drop the (unused) handler value
  currFrame.values.push(result)
  fiber.handlerStack.pop()
  return minorStep
}
