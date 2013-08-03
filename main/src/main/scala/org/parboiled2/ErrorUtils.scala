package org.parboiled2

object ErrorUtils {
  def formatError(input: ParserInput, error: ParseError): String = {
    val ParseError(Position(index, line, col), ruleStacks) = error
    val problem =
      if (index < input.length) s"Invalid input '${input charAt index}'"
      else "Unexpected end of input"

    problem + ", "
    s"expected ${ruleStacks.map(x ⇒ RuleStack(x.frames.reverse)) mkString ("\n", "\n\n", "\n")} " +
      s"(line $line, column $col): \n" +
      s"${input.getLine(line)}\n" + (" " * col) + '^'
  }
}