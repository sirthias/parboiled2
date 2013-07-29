package org.parboiled2

object ErrorUtils {
  def formatError(input: ParserInput, parserError: ParseError): String = {
    // TODO: `parserError.position.column` does not determine cursor at `input`
    val actualChar =
      if (parserError.position.column == input.length) Parser.EOI
      else input.charAt(parserError.position.column)

    s"Invalid input '${actualChar}', " +
      s"expected ${parserError.errorRules.map(x ⇒ RuleStack(x.frames.reverse)) mkString ("\n", "\n\n", "\n")} " +
      s"(line ${parserError.position.line + 1}, pos ${parserError.position.column + 1}): \n" +
      s"${input}\n" + " " * (parserError.position.column) + "^"
  }
}