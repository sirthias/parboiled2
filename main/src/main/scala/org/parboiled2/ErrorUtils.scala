package org.parboiled2

trait ErrorUtils {
  lazy val error: Option[ParserError] = {
    if (targetRule.matched) None
    else {
      parser.collecting = true
      val _ = targetRule.matched
      parser.collecting = false
      val errorMark = parser.errorMark()
      val actualChar =
        if (errorMark.cursor == parser.input.length) parser.EOI
        else parser.input.charAt(errorMark.cursor)
      Some(ParserError(errorMark, actualChar, parser.expectedValues))
    }
  }

  lazy val printError: String = error match {
    case Some(err) ⇒
      s"Invalid input '${err.actualChar}', " +
        s"expected ${err.expectedRules mkString ", "} (line ${err.mark.line + 1}, pos ${err.mark.column + 1}): \n" +
        s"${parser.input}\n" + " " * (err.mark.column) + "^"
    case None ⇒ "No errors"
  }

  def parser: Parser
  def targetRule: Rule
}