# Scamper VM Instruction Set

The Scamper VM instruction set is a variable-length set of opcodes. The first
byte of each opcode is a tag describing the code. The remaining bytes are
arguments to the op as described below. The width of each argument is specified
in bytes.

+ `n`: signed integer
+ `b`: boolean
+ `f`: floating-point number
+ `i`: index (into identifier map, object map, or pattern map)
+ `t`: unsigned instruction offset (from the current index)
+ `k`: unsigned integer

--------------------------------------------------------------------------------
Byte  Instruction       Semantics               
----  ----------------  --------------------------------------------------------
0x00  noop              Skip to next instruction.

0x01  lload <i2> <k1>   Pushes local variable with name found at index `i` of
                        the identifier map onto the value stack at local index
                        `k`.

0x02  lstore <i2> <k1>  Pops the top of the value stack and stores the value as
                        local variable with name found at index `i` of the
                        identifier map at local index `k`.

0x03  gload <i2>        Pushes global variable with name found at index `i` of
                        the identifier map onto the value stack.

0x04  gstore <i2>       Pops the top of the value stack and stores the value as
                        global variable with name found at index `i` of the
                        identifier map.

0x05  int <n4>          Pushes integer `n` onto the value stack.

0x06  bool <b1>         Pushes boolean `b` onto the value stack.

0x07  float <f4>        Pushes float `f` onto the value stack.

0x08  obj <i2>          Pushes object (string, struct, etc.) at index `i` of the
                        object map onto the value stack.

0x09  jmp <t2>          Advances the PC `t` bytes from the start of this op.

0x0a  bjmp <t2>         If the top value of the stack is true, continues to the
                        next instruction. Otherwise, jumps to instruction `t`.

0x0b  match <i2>        Matches the top value of the stack against the patterns
                        found in the pattern table located at index `i` of the
                        object map.

0x0c  ap <n1>           Pops v1, ..., v(n+1) from the value stack and performs
                        The function application v1 ... v(n+1) where v1 is
                        a function.

0x0d  ret               Returns from the current function call, returning the
                        top value of the value stack. The value stack must have
                        exactly one value.

0x0e  add               Pops the top two values from the value stack and pushes
                        the result of applying the given operation to those
                        values.

0x0f  sub

0x10  mult

0x11  div

0x12  lt

0x13  lte

0x14  gt

0x15  gte

0x16  eq

0x17  neq
--------------------------------------------------------------------------------