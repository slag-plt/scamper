/**
 * A `Doc` type is a convenience class for constructing docstrings for libraries
 * written in Javascript
 */
export declare class Doc {
    name: string;
    returnType: string;
    args: ArgDoc[];
    desc: string;
    /**
     * @param name The name of the function.
     * @param returnType the return type of the function.
     * @param args An array of ArgDoc descriptors for each argument of the function.
     * @param desc A prose description of the behavior of the function.
     */
    constructor(name: string, returnType: string, args: ArgDoc[], desc: string);
    /**
     * @returns A string containing the docstring formatted in Markdown.
     */
    docToMarkdown(): string;
}
export declare class ArgDoc {
    name: string;
    desc: string;
    constructor(name: string, desc: string);
    toString(): string;
}
export declare class NoArgDoc extends ArgDoc {
    constructor();
    toString(): string;
}
//# sourceMappingURL=docs.d.ts.map