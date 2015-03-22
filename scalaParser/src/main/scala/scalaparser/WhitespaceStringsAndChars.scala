package scalaparser

import org.parboiled2._

abstract class WhitespaceStringsAndChars extends SimpleParser {
  import L0_Basics._

  implicit val wlStr = rule[String]() { s => WL ~ str(s) }
  implicit val wlCh = rule[Char]() { c => WL ~ ch(c) }
}
