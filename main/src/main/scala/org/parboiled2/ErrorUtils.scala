package org.parboiled2

object ErrorUtils {
  def formatError(parserError: ParseError): String =
    s"Invalid input '${parserError.actualChar}', " +
      s"expected ${parserError.expectedRules mkString ", "} (line ${parserError.line + 1}, pos ${parserError.column + 1}): \n" +
      s"${parserError.input}\n" + " " * (parserError.column) + "^"
}