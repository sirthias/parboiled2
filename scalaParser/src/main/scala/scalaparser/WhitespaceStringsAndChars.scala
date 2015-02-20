package scalaparser

import org.parboiled2._

trait WhitespaceStringsAndChars extends Parser {

  def WL: Rule0

  implicit def wlStr(s: String) = rule( WL ~ str(s) )
  implicit def wlCh(s: Char) = rule( WL ~ ch(s) )
}
