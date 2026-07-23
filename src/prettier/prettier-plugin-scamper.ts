import { Plugin } from 'prettier'
import { SchemeNode } from '../scheme/ast'
import {
  SchemeParser,
  SchemeParserASTFormat,
  SchemeParserName,
} from './scheme/parser'
import { SchemePrinter } from './scheme/printer'

const ScamperPlugin: Plugin<SchemeNode> = {
  languages: [
    {
      name: 'Scamper',
      parsers: ['scamper-scheme'],
    },
  ],
  parsers: {
    [SchemeParserName]: SchemeParser,
  },
  printers: {
    [SchemeParserASTFormat]: SchemePrinter,
  },
}

export default ScamperPlugin
