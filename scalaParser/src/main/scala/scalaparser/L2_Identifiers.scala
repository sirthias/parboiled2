package scalaparser

import org.parboiled2._

trait L2_Identifiers { self: Parser with L0_Basics with L1_KeywordsAndOperators =>

  def VarId                    = rule { WL ~ !Keyword ~ GeneralLower ~ IdRestWithDollar }
  def RawIdNoBackticks         = rule { !Keyword ~ AlphaNum$_   ~ IdRestWithDollar | Operator }
  def RawIdNoDollarNoBackticks = rule { !Keyword ~ AlphaNum$_   ~ IdRest           | Operator }
  def RawId                    = rule { RawIdNoBackticks | '`' ~ (!'`' ~ ANY).+ ~ '`' } // FIXME: are newlines in backticks allowed?
  def Id = rule( WL ~ RawId )
  def Ids = rule( Id.+(WL ~ ',') )

  def StableId: Rule0 = {
    def ClassQualifier = rule( WL ~ '[' ~ Id ~ WL ~ ']' )
    def `.` = rule ( WL ~ '.' )
    def ThisOrSuper = rule( `this` | `super` ~ ClassQualifier.? )
    def ThisOrSuperTail = rule( ThisOrSuper ~ (`.` ~ Id).* )
    rule( Id.+(`.`) ~ (`.` ~ ThisOrSuperTail).? | ThisOrSuperTail )
  }

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def IdRest = rule { (Underscores ~ AlphaNum.+).* ~ OpSuffix }
  private def IdRestWithDollar = rule { (Underscores ~ AlphaNum$.+).* ~ OpSuffix }
  private def Underscores = rule ( ch('_').* )
  private def OpSuffix = rule ( (ch('_').+ ~ OpChar.*).? )
}
