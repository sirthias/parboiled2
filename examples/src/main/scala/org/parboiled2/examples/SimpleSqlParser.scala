package org.parboiled2.examples

import org.parboiled2._

case class tbl(name: String, columns: Seq[String])

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

  def DDL           = rule { Statements.*  }
  def Statements    = rule { Ignore ~ Table }
  def Table         = rule { TableFlag ~ TableName ~ Ignore ~ Arguments ~> tbl }
  def TableName     = rule { capture(!EndName ~ ANY).+ ~> (_.mkString("")) ~ EndName}
  def Arguments     = rule { Arg.*.separatedBy(Ignore) }
  def Arg           = rule { columnStart ~ capture(!Space ~ ANY).+ ~>(_.mkString("")) ~ Space}
  def TableFlag     = rule { CreateTable ~ Space }
  def EndName       = rule { Space | "(" }
  def Ignore        = rule { (! (CreateTable | Space ~ Space)  ~ ANY).+ }
}

object SimpleSqlParser{
  val NewLine = "\r"
  val Comma = ","
  val Space = " "
  val CreateTable = "table"

  val sample =
    """
      |--comment comment
      |
      |create table tables (
      |  id int identity not null,
      |  label varchar(15) not null,
      |  location int not null
      |)
      |
      |create table locations(
      |  id int identity not null,
      |  name varchar(15) not null,
      |  owner varchar(50) not null
      |)
      |
      |-- more comments
    """.stripMargin

  def test = SimpleSqlParser(sample, "  ").DDL.run()

}
