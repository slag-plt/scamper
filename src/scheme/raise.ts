import * as LPM from '../lpm'
import { Fiber } from '../lpm/fiber'
import { Frame } from '../lpm/frame'
import * as A from './ast.js'

/**
 * Lifts an LPM pattern back into a scheme-AST pattern (inverse of codegen's lowerPat).
 * @returns the equivalent scheme-AST pattern
 */
function raisePat(pat: LPM.Pat): A.Pat {
  switch (pat.tag) {
    case 'pwild':
      return A.mkPWild(pat.range)
    case 'plit':
      return A.mkPLit(pat.value, pat.range)
    case 'pvar':
      return A.mkId(pat.name, pat.range)
    case 'pctor':
      return A.mkPCtor(A.mkId(pat.name, pat.range), pat.args.map(raisePat), pat.range)
    case 'pvec':
      return A.mkPVec(pat.args.map(raisePat), pat.range)
  }
}

// The name codegen gives every lambda (see codegen.ts); a `define` replaces it
// when it binds one. A closure still carrying it has no name a student wrote,
// so anything showing the name instead of the lambda is showing an internal
// (#569).
const ANONYMOUS = '##anonymous##'

/** @returns true iff `v` is a closure with no Scamper-facing name. */
export function isAnonymousClosure(v: LPM.Value): v is LPM.Closure {
  return LPM.isClosure(v) && (v.name === undefined || v.name === ANONYMOUS)
}

/**
 * Rebuilds an evaluated closure as the lambda it was written as, by raising its
 * body against the scopes it captured.
 * @param env supplies the top-level names the body references; its locals are
 *            replaced by the closure's own.
 */
export function closureToLam(
  v: LPM.Closure,
  env: LPM.Env = LPM.Env.empty,
): A.Exp {
  const excluded = v.restParam ? [...v.params, v.restParam] : v.params
  return A.mkLam(
    v.params.map((p) => A.mkId(p)),
    raiseFrame(
      [],
      env.withLocalScopes(v.locals).withoutLocals(...excluded),
      v.code.toReversed(),
    ),
    undefined,
    v.restParam ? A.mkId(v.restParam) : undefined,
    v.provenance,
  )
}

/**
 * @param env the frame the values sit in, for raising a closure's body.
 * @return a stack of expressions created from the given value stack.
 */
export function valuesToExps(
  values: LPM.Value[],
  env: LPM.Env = LPM.Env.empty,
): A.Exp[] {
  return values.map((v) => {
    if (isAnonymousClosure(v)) {
      return closureToLam(v, env)
    } else if (LPM.isFunction(v) && v.name) {
      return A.mkId(v.name)
    } else {
      return A.mkLit(v)
    }
  })
}

export function raiseFrame(
  values: A.Exp[],
  env: LPM.Env,
  ops: LPM.Ops[],
): A.Exp {
  for (let i = ops.length - 1; i >= 0; i--) {
    const op = ops[i]
    switch (op.tag) {
      case 'lit': {
        values.push(A.mkLit(op.value, op.range, op.provenance))
        break
      }

      case 'var': {
        const r = env.lookup(op.name)
        if (r.found && r.slot !== LPM.HOLE && !LPM.isFunction(r.slot)) {
          // A bound non-function value: substitute it (shows the value in
          // traces, e.g. a let binder that has already been filled).
          values.push(A.mkLit(r.slot))
        } else {
          // Unbound, a still-unassigned hole, or a function: show the name.
          values.push(A.mkId(op.name))
        }
        break
      }

      case 'cls': {
        // A lambda that has not run yet shows as the lambda it is, unless it
        // has a name to show instead. Codegen's is the ANONYMOUS placeholder,
        // never a name a student wrote, and showing it hid the whole step
        // (#569). The provenance rides along so sugaring recovers a `#(...)`
        // written as one.
        if (op.name !== undefined && op.name !== ANONYMOUS) {
          values.push(A.mkId(op.name))
          break
        }
        const excluded = op.restParam ? [...op.params, op.restParam] : op.params
        values.push(
          A.mkLam(
            op.params.map((p) => A.mkId(p)),
            raiseFrame([], env.withoutLocals(...excluded), op.body.toReversed()),
            op.range,
            op.restParam ? A.mkId(op.restParam) : undefined,
            op.provenance,
          ),
        )
        break
      }

      case 'ap': {
        const vs = values.splice(-(op.numArgs + 1))
        const head = vs[0]
        const args = op.numArgs === 0 ? [] : vs.slice(1)
        values.push(A.mkApp(head, args, op.range, op.provenance))
        break
      }

      case 'match': {
        const scrutinee = LPM.popRequired(values, 'the raise stack')
        const matches = op.branches.map(([pat, body]) => {
          const bodyExp = raiseFrame(
            [],
            env.withoutLocals(...LPM.patVars(pat)),
            body.toReversed(),
          )
          return { pat: raisePat(pat), body: bodyExp }
        })
        values.push(A.mkMatch(scrutinee, matches))
        break
      }

      case 'let': {
        // Reconstruct the let so a trace shows per-binding progress: bindings
        // already assigned are omitted (their values substitute into what
        // remains, via the env); the binding in flight shows its current value
        // (reconstructed on the stack); pending bindings show their original
        // value expressions. Still-unassigned binders are excluded so they
        // render as names rather than substituted values or holes.
        if (op.idx === 0) {
          const excl = env.withoutLocals(
            ...op.bindings.flatMap((b) => LPM.patVars(b.pat)),
          )
          const bindings = op.bindings.map((b) => ({
            pat: raisePat(b.pat),
            value: raiseFrame([], excl, b.value.toReversed()),
          }))
          values.push(
            A.mkLet(
              bindings,
              raiseFrame([], excl, op.body.toReversed()),
              op.range,
              op.provenance,
            ),
          )
        } else {
          const currentValue = LPM.popRequired(values, 'the raise stack')
          const remaining = op.bindings.slice(op.idx - 1)
          const excl = env.withoutLocals(
            ...remaining.flatMap((b) => LPM.patVars(b.pat)),
          )
          const bindings = remaining.map((b, i) => ({
            pat: raisePat(b.pat),
            value:
              i === 0 ? currentValue : raiseFrame([], excl, b.value.toReversed()),
          }))
          values.push(
            A.mkLet(
              bindings,
              raiseFrame([], excl, op.body.toReversed()),
              op.range,
              op.provenance,
            ),
          )
        }
        break
      }

      case 'if': {
        const guard = LPM.popRequired(values, 'the raise stack')
        const thenExp = raiseFrame([], env, op.thenB.toReversed())
        const elseExp = raiseFrame([], env, op.elseB.toReversed())
        values.push(A.mkIf(guard, thenExp, elseExp, op.range, op.provenance))
        break
      }

      case 'hole': {
        // Only ever reached for a hole that has not run yet -- running one
        // raises -- so it reconstructs as the `??` still standing in the
        // source.
        values.push(A.mkHole(op.range))
        break
      }

      case 'pop-scope': {
        // Runtime scope bookkeeping only; nothing to reconstruct.
        break
      }

      case 'ap-spread': {
        const [fn, args] = values.splice(-2)
        values.push(A.mkApp(A.mkId('apply'), [fn, args]))
        break
      }

      case 'push-handler': {
        // N.B., no-op: the handler value stays on the reconstruction stack (it
        // was left there at runtime too) and is consumed by pop-handler below.
        break
      }

      case 'pop-handler': {
        // with-handler is now an ordinary procedure, reconstructed at its call
        // site, so these bracketing ops are transparent to reconstruction: drop
        // the (peeked) handler value, leaving the guarded result.
        const result = LPM.popRequired(values, 'the raise stack')
        values.pop()
        values.push(result)
        break
      }

    }
  }
  return LPM.popRequired(values, 'the raise stack')
}

export function raiseFrames(frames: Frame[]): A.Exp {
  if (frames.length === 0) {
    throw new LPM.ICE('raiseFrames', 'no frames to raise')
  }
  const lastFrame = frames[frames.length - 1]
  let ret = raiseFrame(
    valuesToExps(lastFrame.values, lastFrame.env),
    lastFrame.env,
    lastFrame.ops,
  )
  for (let i = frames.length - 2; i >= 0; i--) {
    const values = valuesToExps(frames[i].values, frames[i].env)
    values.push(ret)
    ret = raiseFrame(values, frames[i].env, frames[i].ops)
  }
  return ret
}

export function raiseFiber(fiber: Fiber): A.Exp {
  return raiseFrames(fiber.frames)
}

///// Frame contexts ///////////////////////////////////////////////////////////
//
// SCAFFOLDING for #494, alongside the raiseFiber above rather than in place of
// it. Nothing in the app calls any of it yet.
//
// `raiseFiber` rebuilds every frame on the stack on every step, which is what
// makes collecting a trace quadratic. But only the *top* frame changes from one
// step to the next (Fiber.stepFrame guards it), so the outer frames' work is
// the same work, redone. The shape below is the same reconstruction with that
// seam left in: each outer frame becomes a `FrameContext` -- its rendering with
// a hole where the callee's value will go -- and `plug` folds them back into the
// one expression `raiseFiber` would have returned.
//
// Stage 3 only proves the two agree (test/scheme/raise-spine.test.ts). Stage 4
// is what will keep a spine across steps, reusing every `FrameContext` whose
// frame is unchanged (`frame` and `version` are what say so) and rebuilding only
// the head; stage 5 is what will sugar a context once instead of once per step.

/**
 * The placeholder a `FrameContext` carries where its callee's value belongs.
 *
 * Deliberately *not* `A.mkHole()`. That is the student's `??` -- a real form
 * with its own meaning, pinned by test/scheme/hole.test.ts -- and a context
 * built from one would be indistinguishable from it in both the tree and the
 * text. `plug` matches by identity, so it would not itself confuse the two, but
 * everything downstream reads the rendering: a context that leaked into a step
 * would show a student a `??` they never wrote.
 *
 * A single shared identifier instead, in the `##...##` shape no program may
 * bind or name (#336, #532). A leak is then visibly an internal -- and
 * harmless, since `visibleReduction` (src/scheme/trace.ts) already drops any
 * step whose text contains `##`. Matching is still by *object* identity, so
 * anything that copies an expression on the way through is a loud failure (an
 * ICE for a context with no hole) rather than a silent one.
 */
const contextHole: A.Exp = A.mkId('##context-hole##')

/**
 * One frame of a raised stack: its own reconstruction, with a hole where the
 * frame it called will go.
 *
 * `frame` and `version` identify the machine state this was built from, so a
 * later step can tell whether it is still current (see Frame.version). `rest`
 * is the frames outside this one, and is meant to be *shared* between
 * consecutive steps rather than rebuilt.
 */
export interface FrameContext {
  readonly ctx: A.Exp
  readonly frame: Frame
  readonly version: number
  readonly rest: Spine
}

/** A stack of frame contexts, innermost first; `null` is the empty stack. */
export type Spine = FrameContext | null

/**
 * @returns `ctx` with its context hole replaced by `inner`.
 * @throws ICE if `ctx` has no hole, which means something copied it.
 */
function fillHole(ctx: A.Exp, inner: A.Exp): A.Exp {
  let filled = false
  // Rebuilt by spread rather than by the mk* constructors: a node's
  // `provenance` is what sugaring reads to recover the derived form it came
  // from, and not every constructor takes one.
  const go = (e: A.Exp): A.Exp => {
    if (e === contextHole) {
      filled = true
      return inner
    }
    switch (e.tag) {
      case 'lit':
      case 'id':
      case 'hole':
        return e
      case 'app':
        return { ...e, head: go(e.head), args: e.args.map((a) => go(a)) }
      case 'lam':
      case 'anonfn':
        return { ...e, body: go(e.body) }
      case 'let':
        return {
          ...e,
          bindings: e.bindings.map((b) => ({ ...b, value: go(b.value) })),
          body: go(e.body),
        }
      case 'begin':
      case 'and':
      case 'or':
      case 'vec':
        return { ...e, exps: e.exps.map((x) => go(x)) }
      case 'if':
        return { ...e, guard: go(e.guard), ifB: go(e.ifB), elseB: go(e.elseB) }
      case 'match':
        return {
          ...e,
          scrutinee: go(e.scrutinee),
          branches: e.branches.map((b) => ({ ...b, body: go(b.body) })),
        }
      case 'cond':
        return {
          ...e,
          branches: e.branches.map((b) => ({
            ...b,
            test: go(b.test),
            body: go(b.body),
          })),
        }
      case 'obj':
        return {
          ...e,
          pairs: e.pairs.map((p) => ({ key: go(p.key), value: go(p.value) })),
        }
    }
  }
  const out = go(ctx)
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition -- `filled` is set inside `go`, which flow analysis does not follow
  if (!filled) {
    throw new LPM.ICE(
      'fillHole',
      'a frame context with no hole to plug: something copied it',
    )
  }
  return out
}

/**
 * Folds `inner` back out through the frames that called it.
 * @returns the whole stack as one expression -- what `raiseFrames` returns.
 */
export function plug(spine: Spine, inner: A.Exp): A.Exp {
  let exp = inner
  for (let s = spine; s !== null; s = s.rest) {
    exp = fillHole(s.ctx, exp)
  }
  return exp
}

/**
 * Reconstructs one frame with a hole where the frame it calls will go.
 * @param rest the contexts outside this one, which become its `rest`.
 */
export function raiseFrameContext(frame: Frame, rest: Spine): FrameContext {
  // The callee's value lands on top of this frame's value stack, exactly where
  // raiseFrames pushes the expression it raised from the frame below.
  const values = valuesToExps(frame.values, frame.env)
  values.push(contextHole)
  return {
    ctx: raiseFrame(values, frame.env, frame.ops),
    frame,
    version: frame.version,
    rest,
  }
}

/**
 * As `raiseFrames`, but keeping the stack's shape: the innermost frame as an
 * expression, and every outer frame as a `FrameContext`. `plug` puts them back
 * together, so `expToString(plug(spine, inner))` is `expToString(raiseFrames(frames))`.
 */
export function raiseSpine(frames: Frame[]): { spine: Spine; inner: A.Exp } {
  if (frames.length === 0) {
    throw new LPM.ICE('raiseSpine', 'no frames to raise')
  }
  let spine: Spine = null
  // Outermost frame first, so each becomes the `rest` of the one it called and
  // the resulting chain runs innermost-first.
  for (let i = 0; i < frames.length - 1; i++) {
    spine = raiseFrameContext(frames[i], spine)
  }
  const innermost = frames[frames.length - 1]
  return {
    spine,
    inner: raiseFrame(
      valuesToExps(innermost.values, innermost.env),
      innermost.env,
      innermost.ops,
    ),
  }
}

/** {@link raiseSpine} of a fiber's frame stack. */
export function raiseFiberSpine(fiber: Fiber): { spine: Spine; inner: A.Exp } {
  return raiseSpine(fiber.frames)
}
