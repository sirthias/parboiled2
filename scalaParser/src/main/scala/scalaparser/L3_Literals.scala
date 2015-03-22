package scalaparser

import org.parboiled2._

trait L3_Literals { this: SimpleParser =>
  import CharacterClasses._
  import L0_Basics._
  import L1_KeywordsAndOperators._
  import L2_Identifiers._

  val Block: Rule0

  val Literal = rule( ("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null )

  val Float = {
    val Exp = rule ( `Ee` ~ `+-`.? ~ DecNum )
    val Decimals = rule( '.' ~ DIGIT.+ ~ Exp.? ~ `FfDd`.? )
    rule( Decimals | DIGIT.+ ~ (Decimals | Exp ~ `FfDd`.? | `FfDd`) )
  }

  val Int = rule( (HexNum | DecNum) ~ `Ll`.? )

  val Bool = rule( True | False )

  val Char = rule ( "'" ~ (UnicodeEscape | EscapedChars | !'\\' ~ test(isPrintableChar(state.cursorChar)) ~ ANY) ~ "'" )

  val Symbol = rule( ''' ~ (RawIdNoBackticks | Keyword) ) // symbols can take on the same values as keywords!

  val String = {
    val TripleTail = rule( '"' ~ '"' ~ ch('"').+ )
    val Inter = rule ( '$' ~ (RawIdNoDollarNoBackticks | '{' ~ Block ~ WL ~ '}' | '$') )
    val Raw = rule { '"'.? ~ '"'.? ~ !'\"' ~ ANY }
    val Simple = rule { '\\' ~ DQBS | !DQLF ~ ANY }
    rule(
      RawId ~ '"' ~ ('"' ~ '"' ~ (Inter | Raw).* ~ TripleTail | (Inter | Simple).* ~ '"')
    | '"' ~ ('"' ~ '"' ~ Raw.* ~ TripleTail | Simple.* ~ '"'))
  }

  val WLLiteral = rule( WL ~ Literal )

  //////////////////////////// PRIVATE ///////////////////////////////////

  private val UnicodeEscape = rule { "\\u" ~ HEXDIGIT ~ HEXDIGIT ~ HEXDIGIT ~ HEXDIGIT }

  private val EscapedChars = rule( '\\' ~ ESCAPEE )

  private def isPrintableChar(c: Char): Boolean =
    !Character.isISOControl(c) &&
    !Character.isSurrogate(c) &&
    { val block = Character.UnicodeBlock.of(c); block != null && block != Character.UnicodeBlock.SPECIALS }
}
