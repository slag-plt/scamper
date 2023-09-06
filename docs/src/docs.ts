/**
 * A `Doc` type is a convenience class for constructing docstrings for libraries
 * written in Javascript
 */
export default class Doc {
  /**
   *
   * @param sig A docstring corresponding to the signature of the function.
   * @param args An array of docstrings for each of the function's arguments.
   * @param desc A prose description of the behavior of the function.
   */
  // eslint-disable-next-line no-useless-constructor
  constructor (public sig: string, public args: string[], public desc: string) { }

  /**
   * @returns A string containing the docstring formatted in Markdown.
   */
  public docToMarkdown (): string {
    return `
~~~
${this.sig.trim()}

${this.args.map(arg => '  ' + arg.trim()).join('\n')}
~~~

${this.desc.trim()}
  `.trim()
  }
}