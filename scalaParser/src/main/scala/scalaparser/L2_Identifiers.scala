package scalaparser

import org.parboiled2._

object L2_Identifiers extends SimpleParser {
  import L0_Basics._
  import L1_KeywordsAndOperators._

  val VarId                    = rule { WL ~ !Keyword ~ GeneralLower ~ IdRestWithDollar }
  val RawIdNoBackticks         = rule { !Keyword ~ AlphaNum$_   ~ IdRestWithDollar | Operator }
  val RawIdNoDollarNoBackticks = rule { !Keyword ~ AlphaNum$_   ~ IdRest           | Operator }
  val RawId                    = rule { RawIdNoBackticks | '`' ~ (!'`' ~ ANY).+ ~ '`' } // FIXME: are newlines in backticks allowed?
  val Id = rule( WL ~ RawId )
  val Ids = rule( Id.+(WL ~ ',') )

  val StableId: Rule0 = {
    val ClassQualifier = rule( WL ~ '[' ~ Id ~ WL ~ ']' )
    val `.` = rule ( WL ~ '.' )
    val ThisOrSuper = rule( `this` | `super` ~ ClassQualifier.? )
    val ThisOrSuperTail = rule( ThisOrSuper ~ (`.` ~ Id).* )
    rule( Id.+(`.`) ~ (`.` ~ ThisOrSuperTail).? | ThisOrSuperTail )
  }

  //////////////////////////// PRIVATE ///////////////////////////////////

  private val IdRest = rule { (Underscores ~ AlphaNum.+).* ~ OpSuffix }
  private val IdRestWithDollar = rule { (Underscores ~ AlphaNum$.+).* ~ OpSuffix }
  private val Underscores = rule ( ch('_').* )
  private val OpSuffix = rule ( (ch('_').+ ~ OpChar.*).? )
}
