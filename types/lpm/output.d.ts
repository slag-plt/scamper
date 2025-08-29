import { Value } from './lang.js';
import { ScamperError } from './error.js';
/** A "console" that can receive output from LPM. */
export interface OutputChannel {
    send: (v: Value) => void;
}
/** A channel that simply logs any output that it is fed. */
export declare class LoggingOutputChannel implements OutputChannel {
    log: Value[];
    constructor();
    send(v: Value): void;
}
/** A "console" that can receive errors from LPM. */
export interface ErrorChannel {
    report: (e: ScamperError) => void;
}
/** An error channel that simply logs any errors that arise. */
export declare class LoggingErrorChannel implements ErrorChannel {
    log: ScamperError[];
    constructor();
    report(err: ScamperError): void;
}
/** A unified output and error channel that logs by line */
export declare class LoggingChannel implements OutputChannel, ErrorChannel {
    log: string[];
    constructor();
    send(v: Value): void;
    report(e: ScamperError): void;
}
//# sourceMappingURL=output.d.ts.map