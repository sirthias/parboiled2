package scalaparser

import org.parboiled2._

object L0_Basics extends SimpleParser {
  import CharacterClasses._

  val HexNum = rule { "0x" ~ HEXDIGIT.+ }

  val DecNum = rule { DIGIT.+ }

  val Newline = rule( quiet( '\r'.? ~ '\n' ) )

  val LineEnd = rule( quiet ( WL ~ Newline ) )

  val OpChar = rule( atomic( OPCHAR | test(isMathOrOtherSymbol(state.cursorChar)) ~ ANY ) )

  val AlphaNum = rule { ALPHANUM |GeneralAlphaNum }
  val AlphaNum$ = rule { ALPHANUM$ | GeneralAlphaNum }
  val AlphaNum$_ = rule { ALPHANUM$_ | GeneralAlphaNum }

  val GeneralLower = rule( atomic( `LOWER$_` | test(state.cursorChar.isLower) ~ ANY ) )

  /**
   * Whitespace, including newlines. This is the default for most things.
   */
  val WL = rule( quiet( (WSCHAR | Comment | Newline).* ) )

  /**
   * Whitespace, excluding newlines.
   * Only really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference works
   */
  val WS = rule( quiet (WSCHAR | Comment).* )

  val Semi = rule( WL ~ ';' | WS ~ Newline.+ )

  val Semis = rule( Semi.+ )

  val NotNewline: Rule0 = rule( &( WS ~ !Newline ) )

  val OneNLMax: Rule0 = rule( quiet ( WS ~ Newline.? ~ CommentLine.* ~ NotNewline ) )

  //////////////////////////// PRIVATE ///////////////////////////////////

  private val Comment: Rule0 = rule( BlockComment | "//" ~ (!Newline ~ ANY).* )

  private val BlockComment: Rule0 = rule( "/*" ~ (BlockComment | !"*/" ~ ANY).* ~ "*/" )

  private val GeneralAlphaNum = rule ( test(state.cursorChar.isLetter | state.cursorChar.isDigit) ~ ANY )

  private val CommentLine = rule( quiet( WSCHAR.* ~ Comment ~ WSCHAR.* ~ Newline ) )

  private def isMathOrOtherSymbol(c: Char) =
    Character.getType(c) match {
      case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true
      case _ => false
    }
}
