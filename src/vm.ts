import * as Ops from './ops.js'
import {
  Closure, CompiledProgram, Env, isClosure, isJsFunction, OpIdx, Value
} from './value.js'

type Frame = {
  pc: OpIdx,
  values: Value[],
  env: Env
}

type MachineState = {
  program: CompiledProgram,
  globals: Map<string, Value>,
  currentFrame: Frame,
  dump: Frame[]
}

type Step = { tag: 'step' }
const stepResult: Step = { tag: 'step' }

type Result = { tag: 'result', value: Value }

function result (value: Value): Result {
  return { tag: 'result', value }
}

type StepResult = Step | Result

function stepBinop (state: MachineState, op: (x: number, y: number) => Value, opText: string, opSize: number): StepResult {
  // TODO: assert that the value stack contains at least two values
  const x = state.currentFrame.values.pop()
  const y = state.currentFrame.values.pop()
  // TODO: check whether each argument is indeed a number
  state.currentFrame.values.push(op(x as number, y as number))
  state.currentFrame.pc += opSize
  return stepResult
}

function step (state: MachineState): StepResult {
  const instr = state.program.ops.getUint8(state.currentFrame.pc)
  switch (instr) {
    case Ops.Tag.NOOP: {
      state.currentFrame.pc += Ops.Size.NOOP
      return stepResult
    }
    
    case Ops.Tag.LLOAD: {
      const i = state.program.ops.getUint8(state.currentFrame.pc + 3)
      state.currentFrame.values.push(state.currentFrame.env[i])
      state.currentFrame.pc += Ops.Size.LLOAD
      return stepResult
    }

    case Ops.Tag.LSTORE: {
      const i = state.program.ops.getUint8(state.currentFrame.pc + 3)
      // TOOD: throw an explicit runtime error here if the value stack is
      // empty
      const value = state.currentFrame.values.pop()
      state.currentFrame.env[i] = value
      state.currentFrame.pc += Ops.Size.LSTORE
      return stepResult
    }

    case Ops.Tag.GLOAD: {
      // TODO: throw an error here if the index is out of bounds
      const i = state.program.ops.getUint16(state.currentFrame.pc + 1)
      // TODO: throw an error if the identifier is not found
      const x = state.program.identifiers[i]
      // TODO: throw a runtime error if the global is not found
      state.currentFrame.values.push(state.globals.get(x)!)
      state.currentFrame.pc += Ops.Size.GLOAD
      return stepResult
    }
        
    case Ops.Tag.GSTORE: {
      // TODO: throw an error here if the index is out of bounds
      const i = state.program.ops.getUint16(state.currentFrame.pc + 1)
      // TODO: throw an error if the identifier is not found
      const x = state.program.identifiers[i]
      // TODO: throw an error if the value stack is empty
      const value = state.currentFrame.values.pop()
      state.globals.set(x, value)
      state.currentFrame.pc += Ops.Size.INT
      return stepResult
    }

    case Ops.Tag.INT: {
      const value = state.program.ops.getInt32(state.currentFrame.pc + 1)
      state.currentFrame.values.push(value)
      state.currentFrame.pc += Ops.Size.INT
      return stepResult
    }

    case Ops.Tag.BOOL: {
      const value = state.program.ops.getUint8(state.currentFrame.pc + 1) == 0 ? false : true
      state.currentFrame.values.push(value)
      state.currentFrame.pc += Ops.Size.BOOL
      return stepResult
    }

    case Ops.Tag.FLOAT: {
      const value = state.program.ops.getFloat32(state.currentFrame.pc + 1)
      state.currentFrame.values.push(value)
      state.currentFrame.pc += Ops.Size.FLOAT
      return stepResult
    }

    case Ops.Tag.OBJ: {
      const i = state.program.ops.getUint16(state.currentFrame.pc + 1)
      state.currentFrame.values.push(state.program.objects[i])
      state.currentFrame.pc += Ops.Size.OBJ
      return stepResult
    }

    case Ops.Tag.JMP: {
      const di = state.program.ops.getUint16(state.currentFrame.pc + 1)
      state.currentFrame.pc += Ops.Size.JMP + di
      return stepResult
    }

    case Ops.Tag.BJMP: {
      // TODO: check if the value stack is empty
      const isJumping = state.currentFrame.values.pop() 
      const di = state.program.ops.getUint16(state.currentFrame.pc + 1)
      state.currentFrame.pc += Ops.Size.BJMP + (isJumping ? di : 0)
      return stepResult
    }

    case Ops.Tag.MATCH: {
      // TODO: implement me!
      throw new Error('MATCH not implemented')
    }

    case Ops.Tag.AP: {
      const numArgs = state.program.ops.getUint8(state.currentFrame.pc + 1)
      // TODO: check if the value stack has numArgs + 1 values
      const fn = state.currentFrame.values.pop()
      const args = state.currentFrame.values.splice(-numArgs)
      if (isJsFunction(fn)) {
        const result = (fn as Function)(...args)
        state.currentFrame.values.push(result)
        state.currentFrame.pc += Ops.Size.AP
      } else if (isClosure(fn)) {
        const closure = fn as Closure
        const newFrame = {
          pc: closure.idx,
          values: [],
          env: args
        }
        state.currentFrame.pc += Ops.Size.AP
        state.dump.push(state.currentFrame)
        state.currentFrame = newFrame
      } else {
        // TODO: raise a user-facing runtime error
        throw new Error('AP: expected a function or closure')
      }
      return stepResult
    }

    case Ops.Tag.RET: {
      // TODO: assert that the value stack contains exactly one value
      const ret = state.currentFrame.values.pop()
      if (state.dump.length === 0) {
        return result(ret)
      } else {
        const frame = state.dump.pop()!
        state.currentFrame = frame
      }
      return stepResult
    }

    case Ops.Tag.ADD: {
      return stepBinop(state, (x, y) => x + y, '+', Ops.Size.ADD)
    }

    case Ops.Tag.SUB: {
      return stepBinop(state, (x, y) => x - y, '-', Ops.Size.SUB)
    }

    case Ops.Tag.MULT: {
      return stepBinop(state, (x, y) => x * y, '*', Ops.Size.MULT)
    }

    case Ops.Tag.DIV: {
      return stepBinop(state, (x, y) => x / y, '/', Ops.Size.DIV)
    }

    case Ops.Tag.LT: {
      return stepBinop(state, (x, y) => x < y ? true : false, '<', Ops.Size.LT)
    }

    case Ops.Tag.LTE: {
      return stepBinop(state, (x, y) => x <= y ? true : false, '<=', Ops.Size.LTE)
    }

    case Ops.Tag.GT: {
      return stepBinop(state, (x, y) => x > y ? true : false, '>', Ops.Size.GT)
    }

    case Ops.Tag.GTE: {
      return stepBinop(state, (x, y) => x >= y ? true : false, '>=', Ops.Size.GTE)
    }

    case Ops.Tag.EQ: {
      return stepBinop(state, (x, y) => x === y ? true : false, '===', Ops.Size.EQ)
    }

    case Ops.Tag.NEQ: {
      return stepBinop(state, (x, y) => x !== y ? true : false, '!==', Ops.Size.NEQ)
    }

    default: {
      throw new Error(`ICE: unknown opcode: {instr}`)
    }
  }
}