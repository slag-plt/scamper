/** A mapping of opcodes to their corresponding bytecode values. */
declare const Ops: {
    noop: number;
    ifnb: number;
    ifnm: number;
    jmp: number;
    ap: number;
    ret: number;
    disp: number;
    int: number;
    bool: number;
    str: number;
    obj: number;
    lload: number;
    lstore: number;
    gload: number;
    gstore: number;
    add: number;
    sub: number;
    mul: number;
    div: number;
    lt: number;
    lte: number;
    gt: number;
    gte: number;
    eq: number;
    neq: number;
    loadlib: number;
};
export default Ops;
//# sourceMappingURL=ops.d.ts.map