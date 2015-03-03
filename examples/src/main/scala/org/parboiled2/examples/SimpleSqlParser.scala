package org.parboiled2.examples

import org.parboiled2._

case class DbTable(name: String, columns: Seq[String])

/**
 * Shows how easy it is to express a grammar in pseudo-BNF using Parboiled2.
 * Specifically, this highlights how easy it is to build up a targeted parser to extract specific elements from a document.
 *
 * Extracts SQL tables & column names as tbl object from a create table *.sql file
 * @param input
 * @param columnStart
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


  /* Explain what the parser consists of:
      Terminal symbols: NewLine, Comma, Space, & Create table, ANY, "("
      NonTerminal Symbol: Ignore, EndName, TableFlag, Arg, Arguments, TableName, Table, statements, DDL

      
   */


}
