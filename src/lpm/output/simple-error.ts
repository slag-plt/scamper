import {ErrorChannel} from "./channel";
import {ScamperError, SubthreadErrors} from "../error";

export class SimpleErrorChannel implements ErrorChannel {
    private readonly _errors: ScamperError[]

    constructor(errors?: ScamperError[]) {
        this._errors = [];
        if (errors) {
            this._errors.push(...errors);
        }
    }

    report(e: ScamperError) {
        this._errors.push(e)
    }

    get errors() {
        return [...this._errors]
    }

    getSubthreadErrors() {
        return new SubthreadErrors(this._errors)
    }
}