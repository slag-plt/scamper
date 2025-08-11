import { ScamperError, Value } from '../lpm/runtime.js';
export declare function checkSyntaxSingle(errors: ScamperError[], v: Value): void;
export declare function checkSyntaxExpr(errors: ScamperError[], v: Value): void;
export declare function checkSyntaxStmt(errors: ScamperError[], v: Value): void;
export declare function checkSyntaxProgram(program: Value[]): ScamperError[];
//# sourceMappingURL=syntax.d.ts.map