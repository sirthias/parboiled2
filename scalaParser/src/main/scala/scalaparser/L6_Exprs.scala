package scalaparser

import org.parboiled2._

trait L6_Exprs { this: Parser with WhitespaceStringsAndChars
  with L0_Basics
  with L1_KeywordsAndOperators
  with L4_Core
  with L5_Types
  with L5_Xml =>

  def NewBody: Rule0
  def BlockDef: Rule0

  def Import: Rule0 = {
    def ImportExpr: Rule0 = rule( StableId ~ ('.' ~ (`_` | Selectors)).? )
    def Selectors: Rule0 = rule( '{' ~ (Selector ~ ',').* ~ (Selector | `_`) ~ "}" )
    def Selector: Rule0 = rule( WLId ~ (`=>` ~ (WLId | `_`)).? )
    rule( `import` ~ ImportExpr.+(',') )
  }

  def Ascription = rule( `:` ~ (`_*` |  Type | Annot.+) )

  def LambdaHead: Rule0 = {
    def Binding = rule( (WLId | `_`) ~ (`:` ~ Type).? )
    def Bindings = rule( '(' ~ Binding.*(',') ~ ')' )
    def Implicit = rule( `implicit`.? ~ WLId ~ (`:` ~ InfixType).? )
    rule( (Bindings | Implicit | `_` ~ Ascription.?) ~ `=>` )
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  def TypeExpr = ExprCtx.Expr
  class WsCtx(injectSemicolons: Boolean){

    def OneSemiMax = if (injectSemicolons) OneNLMax else MATCH
    def NoSemis = if (injectSemicolons) NotNewline else MATCH

    def Enumerators = {
      def Generator = rule( Pat1 ~ `<-` ~ Expr ~ Guard.? )
      def Assign = rule( Pat1 ~ `=` ~ Expr )
      def Enumerator = rule( Semis ~ Generator | optional(Semis) ~ Guard | Semis ~ Assign )
      rule( Generator ~ Enumerator.* ~ WL )
    }
    def Expr: Rule0 = {
      def If = {
        def Else = rule( Semi.? ~ `else` ~ Expr )
        rule( `if` ~ '(' ~ ExprCtx.Expr ~ ')' ~ Expr ~ Else.? )
      }
      def While = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr )
      def Try = {
        def Catch = rule( `catch` ~ Expr )
        def Finally = rule( `finally` ~ Expr )
        rule( `try` ~ Expr ~ Catch.? ~ Finally.? )
      }
      def DoWhile = rule( `do` ~ Expr ~ Semi.? ~ `while` ~ '(' ~ Expr ~ ")" )

      def For = {
        def Body = rule( '(' ~ ExprCtx.Enumerators ~ ')' | '{' ~ StatCtx.Enumerators ~ '}' )
        rule( `for` ~ Body ~ `yield`.? ~ Expr )
      }
      def Throw = rule( `throw` ~ Expr )
      def Return = rule( `return` ~ Expr.? )

      def SmallerExpr = rule( PostfixExpr ~ (`match` ~ '{' ~ CaseClauses ~ "}" | Ascription).? )
      def LambdaBody = rule( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
      rule( LambdaHead.* ~ LambdaBody )
    }

    def PostfixExpr: Rule0 = {
      def Prefixed = rule( (WL ~ anyOf("-+~!") ~ WS ~ !OpChar) ~  SimpleExpr )
      def Assign = rule( SimpleExpr ~ (`=` ~ Expr).? )
      def PrefixExpr = rule( Prefixed | Assign )

      def InfixExpr = rule( PrefixExpr ~ (NoSemis ~ WLId ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr).* )
      rule( InfixExpr ~ (NotNewline ~ WLId ~ WLNewline.?).? )
    }

    def SimpleExpr: Rule0 = {
      def Path = rule( (WLId ~ '.').* ~ `this` ~ ('.' ~ WLId).* | StableId )
      def New = rule( `new` ~ NewBody )
      def Parened = rule ( '(' ~ Exprs.? ~ ")"  )
      def SimpleExpr1 = rule( XmlExpr | New | BlockExpr | WLLiteral | Path | `_` | Parened)
      rule( SimpleExpr1 ~ ('.' ~ WLId | TypeArgs | NoSemis ~ ArgList).* ~ (NoSemis  ~ `_`).?)
    }
    def Guard : Rule0 = rule( `if` ~ PostfixExpr )
  }
  def SimplePat: Rule0 = {
    def ExtractorArgs = rule( Pat.*(',') )
    def Extractor = rule( StableId ~ ('(' ~ ExtractorArgs ~ ')').? )
    def TupleEx = rule( '(' ~ ExtractorArgs.? ~ ')' )
    def Thingy = rule( `_` ~ (`:` ~ TypePat).? ~ !"*" )
    rule( XmlPattern | Thingy | WLLiteral | TupleEx | Extractor | WLVarId)
  }

  def BlockExpr: Rule0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  def BlockStats: Rule0 = {
    def Prelude = rule( Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.* )
    def Tmpl = rule( Prelude ~ BlockDef )
    def BlockStat = rule( Import | Tmpl | StatCtx.Expr )
    rule( BlockStat.+(Semis) )
  }

  def Block: Rule0 = {
    def End = rule( Semis.? ~ &("}" | `case`) )
    def ResultExpr = rule{ StatCtx.Expr | LambdaHead ~ Block}
    def Body = rule( ResultExpr ~ End | BlockStats ~ (Semis ~ ResultExpr).? ~ End | End )
    rule( LambdaHead.* ~ Semis.? ~ Body )
  }

  def Patterns: Rule0 = rule( Pat.+(",") )
  def Pat: Rule0 = rule( Pat1.+('|') )
  def Pat1: Rule0 = rule( `_` ~ `:` ~ TypePat | WLVarId ~ `:` ~ TypePat | Pat2 )
  def Pat2: Rule0 = {
    def Pat3 = rule( `_*` | SimplePat ~ (WLId ~ SimplePat).* )
    rule( WLVarId ~ `@` ~ Pat3 | Pat3 | WLVarId )
  }

  def TypePat = rule( CompoundType )

  def ArgList: Rule0 = rule( '(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")" | OneNLMax ~ BlockExpr )

  def CaseClauses: Rule0 = {
    def CaseClause: Rule0 = rule( `case` ~ Pat ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    rule( CaseClause.+ )
  }
}
