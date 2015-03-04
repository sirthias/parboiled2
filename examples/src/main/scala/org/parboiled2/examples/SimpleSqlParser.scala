package org.parboiled2.examples

import org.parboiled2._

case class DbTable(name: String, columns: Seq[String])

/* Often times parsing a document correctly means discarding slices of input along the way, i.e. code comments.
 *
 * This example illustrates how to create a parser that extracts specific slices from a document while discarding 
 * extraneous slices.
 */

case class SimpleSqlParser(input: ParserInput, columnStart: String) extends Parser {
  import SimpleSqlParser._

  def DDL           = rule { zeroOrMore(Statements) }
  def Statements    = rule { Ignore ~ Table }
  def Table         = rule { TableFlag ~ TableName ~ Ignore ~ Arguments ~> DbTable }
  def TableName     = rule { capture(oneOrMore(!EndName ~ ANY)) ~ EndName}
  def Arguments     = rule { zeroOrMore(Arg).separatedBy(Ignore) }
  def Arg           = rule { columnStart ~ capture(oneOrMore(!Space ~ ANY)) ~ Space}
  def TableFlag     = rule { CreateTable ~ Space }
  def EndName       = rule { Space | "(" }
  def Ignore        = rule { (! (CreateTable | columnStart)  ~ ANY).+ }
}

object SimpleSqlParser{
  val NewLine = "\r"
  val Comma = ","
  val Space = " "
  val CreateTable = "table"


}
