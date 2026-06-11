export function matchesDocTagFormat(line: string): boolean {
  const splitLine = line.split("@", 2)
  // has an @ && the @ is at the beginning && right after the @ is not a whitespace
  return (
    splitLine.length === 2 && splitLine[0] === "" && /^\w/.test(splitLine[1])
  )
}
