/** A mapping of opcodes to their corresponding bytecode values. */
const Ops = {
  // Instrument argument types:
  // + n: an integer constant
  // + b: a boolean constant (0 for false, 1 for true)
  // + l: a label (index into op sequence)
  // + s: a string (index into the string table)
  // + r: a reference (index) into the object table
  // + i: an index into the environment (i.e., a local variable)

  // Control
  'noop': 0x00,     // noop
  'ifnb': 0x01,     // ifnb l  [1 value]
  'ifnm': 0x02,     // ifnm r  [1 value]
  'jmp': 0x03,      // jmp l   [1 value]
  'ap': 0x04,       // ap n    [n+1 values]
  'ret': 0x05,      // ret     [1 value]
  'disp': 0x06,     // disp    [1 value]

  // Literals
  'int': 0x10,      // int n
  'bool': 0x11,     // bool b
  'str': 0x12,      // str s
  'obj': 0x13,      // obj r

  // Variables
  'lload': 0x20,    // lload i
  'lstore': 0x21,   // lstore i [1 value]
  'gload': 0x22,    // gload s
  'gstore': 0x23,   // gstore s [1 value]

  // Primitives
  'add': 0x30,      // add [2 values]
  'sub': 0x31,      // sub [2 values]
  'mul': 0x32,      // mul [2 values]
  'div': 0x33,      // div [2 values]
  'lt': 0x34,       // lt  [2 values]
  'lte': 0x35,      // lte [2 values]
  'gt': 0x36,       // gt  [2 values]
  'gte': 0x37,      // gte [2 values]
  'eq': 0x38,       // eq  [2 values]
  'neq': 0x39,      // neq [2 values]

  // Runtime
  'loadlib': 0x40,  // loadlib s
}

export default Ops

