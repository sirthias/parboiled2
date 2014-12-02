package scalaparser

import org.parboiled2._

trait L3_Literals { self: Parser with L0_Basics with L1_KeywordsAndOperators with L2_Identifiers =>
  import CharacterClasses._

  def Block: Rule0

  def Literal = rule( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )

  def Float = {
    def Exp = rule ( `Ee` ~ `+-`.? ~ DecNum )
    def Decimals = rule( '.' ~ DIGIT.+ ~ Exp.? ~ `FfDd`.? )
    rule( Decimals | DIGIT.+ ~ (Decimals | Exp ~ `FfDd`.? | `FfDd`) )
  }

  def Int = rule( (HexNum | DecNum) ~ `Ll`.? )

  def Bool = rule( True | False )

  def Char = rule ( "'" ~ (UnicodeEscape | EscapedChars | !'\\' ~ test(isPrintableChar(cursorChar)) ~ ANY) ~ "'" )

  def Symbol = rule( ''' ~ (PlainId | Keyword) ) // symbols can take on the same values as keywords!

  def String = {
    def TripleTail = rule( '"' ~ '"' ~ ch('"').+ )
    def Inter = rule ( '$' ~ (PlainIdNoDollar | '{' ~ Block ~ WL ~ '}' | '$') )
    def Raw = rule { '"'.? ~ '"'.? ~ !'\"' ~ ANY }
    def Simple = rule { '\\' ~ DQBS | !DQLF ~ ANY }
    rule(
      Id ~ '"' ~ ('"' ~ '"' ~ (Inter | Raw).* ~ TripleTail | (Inter | Simple).* ~ '"')
    | '"' ~ ('"' ~ '"' ~ Raw.* ~ TripleTail | Simple.* ~ '"'))
  }

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def UnicodeEscape = rule { "\\u" ~ HEXDIGIT ~ HEXDIGIT ~ HEXDIGIT ~ HEXDIGIT }

  private def EscapedChars = rule( '\\' ~ ESCAPEE )

  private def isPrintableChar(c: Char): Boolean =
    !Character.isISOControl(c) &&
    !Character.isSurrogate(c) &&
    { val block = Character.UnicodeBlock.of(c); block != null && block != Character.UnicodeBlock.SPECIALS }
}
