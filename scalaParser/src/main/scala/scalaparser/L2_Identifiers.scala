package scalaparser

import org.parboiled2._

trait L2_Identifiers { self: Parser with L0_Basics with L1_KeywordsAndOperators =>

  def VarId           = rule { !Keyword ~ GeneralLower ~ IdRestWithDollar }
  def PlainId         = rule { !Keyword ~ AlphaNum$_   ~ IdRestWithDollar | Operator }
  def PlainIdNoDollar = rule { !Keyword ~ AlphaNum$_   ~ IdRest           | Operator }
  def Id = rule { PlainId | '`' ~ (!'`' ~ ANY).+ ~ '`' } // FIXME: are newlines in backticks allowed?

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def IdRest = rule { (Underscores ~ AlphaNum.+).* ~ OpSuffix }
  private def IdRestWithDollar = rule { (Underscores ~ AlphaNum$.+).* ~ OpSuffix }
  private def Underscores = rule ( ch('_').* )
  private def OpSuffix = rule ( (ch('_').+ ~ OpChar.*).? )
}
