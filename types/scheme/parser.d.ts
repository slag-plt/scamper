import * as A from './ast.js';
import * as L from '../lpm';
export declare function parseSingle(errors: L.ScamperError[], v: L.Value, range: L.Range): A.Exp;
export declare function parseExp(errors: L.ScamperError[], v: L.Value): A.Exp;
export declare function parseStmt(errors: L.ScamperError[], v: L.Value): A.Stmt;
export declare function parseProgram(errors: L.ScamperError[], values: L.Value[]): A.Prog;
//# sourceMappingURL=parser.d.ts.map