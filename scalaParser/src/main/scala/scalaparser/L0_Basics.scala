package scalaparser

import org.parboiled2._

trait L0_Basics { this: Parser =>
  import CharacterClasses._

  def HexNum = rule { "0x" ~ HEXDIGIT.+ }

  def DecNum = rule { DIGIT.+ }

  def Newline = rule { '\r'.? ~ '\n' }

  def OpChar = rule { OPCHAR | test(isMathOrOtherSymbol(cursorChar)) ~ ANY }

  def AlphaNum = rule { ALPHANUM |GeneralAlphaNum }
  def AlphaNum$ = rule { ALPHANUM$ | GeneralAlphaNum }
  def AlphaNum$_ = rule { ALPHANUM$_ | GeneralAlphaNum }

  def GeneralLower = rule( `LOWER$_` | test(cursorChar.isLower) ~ ANY )

  def Comment: Rule0 = rule( BlockComment | "//" ~ (!Newline ~ ANY).* )

  /**
   * Whitespace, including newlines. This is the default for most things.
   */
  def WL = rule( (WSCHAR | Comment | Newline).* )

  /**
   * Whitespace, excluding newlines.
   * Only really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference works
   */
  def WS = rule( (WSCHAR | Comment).* )

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def BlockComment: Rule0 = rule( "/*" ~ (BlockComment | !"*/" ~ ANY).* ~ "*/" )

  private def GeneralAlphaNum = rule ( test(cursorChar.isLetter | cursorChar.isDigit) ~ ANY )

  private def isMathOrOtherSymbol(c: Char) =
    Character.getType(c) match {
      case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true
      case _ => false
    }
}
