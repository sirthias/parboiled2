package scalaparser

import org.parboiled2._

class ScalaParser(val input: ParserInput)
    extends Parser with WhitespaceStringsAndChars with L0_Basics with L1_KeywordsAndOperators with L2_Identifiers
    with L3_Literals with L4_Types with L4_Xml with L5_Exprs with L6_TopLevel
