
/**
 * A `Doc` type is a convenience class for constructing docstrings for libraries
 * written in Javascript  
 */
export class Doc {
  /**
   * @param name The name of the function.
   * @param returnType the return type of the function.
   * @param args An array of ArgDoc descriptors for each argument of the function.
   * @param desc A prose description of the behavior of the function.
   */
  // eslint-disable-next-line no-useless-constructor
  constructor (public name: string,
               public returnType: string, 
               public args: ArgDoc[],
               public desc: string) { }

  /**
   * @returns A string containing the docstring formatted in Markdown.
   */
  public docToMarkdown (): string {
    if (this.args.length === 0) {
      return `
~~~
${this.name} : ${this.returnType}
~~~

${this.desc.trim()}
      `.trim()
    } else {
      return `
~~~
(${this.name} ${this.args.map(arg => arg.name).join(' ')}) -> ${this.returnType}
${this.args.map(arg => '  ' + arg.toString()).join('\n')}
~~~

${this.desc.trim()}
    `.trim()
    }
  }
}

export class ArgDoc {
  constructor (public name: string, public desc: string) { }

  public toString(): string {
    return `${this.name}: ${this.desc}`
  }
}