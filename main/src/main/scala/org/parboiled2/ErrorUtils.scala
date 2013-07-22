package org.parboiled2

trait ErrorUtils {
  val parseErrors: String = {
    parser.collecting = true
    val res = targetRule.matched
    assert(res == false)
    val Some(err) = parser.error()
    parser.collecting = false
    s"Invalid input '${err.actual}', " +
      s"expected ${err.expected mkString ", "} (line ${err.mark.line + 1}, pos ${err.mark.column + 1}): \n" +
      s"${parser.input}\n" + " " * (err.mark.column) + "^"
  }

  def parser: Parser
  def targetRule: Rule
}