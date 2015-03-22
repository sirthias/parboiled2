package scalaparser

import org.parboiled2._

trait L5_Exprs { this: WhitespaceStringsAndChars
  with L3_Literals
  with L4_Types
  with L4_Xml =>

  import L0_Basics._
  import L1_KeywordsAndOperators._
  import L2_Identifiers._

  val NewBody: Rule0
  val BlockDef: Rule0

  val Import: Rule0 = {
    val Selector: Rule0 = rule( Id ~ (`=>` ~ (Id | underscore)).? )
    val Selectors: Rule0 = rule( '{' ~ (Selector ~ ',').* ~ (Selector | underscore) ~ "}" )
    val ImportExpr: Rule0 = rule( StableId ~ ('.' ~ (underscore | Selectors)).? )
    rule( `import` ~ ImportExpr.+(',') )
  }

  val Ascription = rule( `:` ~ (`_*` |  Type | Annot.+) )

  val LambdaHead: Rule0 = {
    val Binding = rule( (Id | underscore) ~ (`:` ~ Type).? )
    val Bindings = rule( '(' ~ Binding.*(',') ~ ')' )
    val Implicit = rule( `implicit`.? ~ Id ~ (`:` ~ InfixType).? )
    rule( (Bindings | Implicit | underscore ~ Ascription.?) ~ `=>` )
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  val TypeExpr = ExprCtx.Expr
  class WsCtx(injectSemicolons: Boolean){

    val OneSemiMax = if (injectSemicolons) OneNLMax else rule(MATCH)
    val NoSemis = if (injectSemicolons) NotNewline else rule(MATCH)

    val Enumerators = {
      val Generator = rule( Pat1 ~ `<-` ~ Expr ~ Guard.? )
      val Assign = rule( Pat1 ~ `=` ~ Expr )
      val Enumerator = rule( Semis ~ Generator | optional(Semis) ~ Guard | Semis ~ Assign )
      rule( Generator ~ Enumerator.* ~ WL )
    }
    val Expr: Rule0 = {
      val If = {
        val Else = rule( Semi.? ~ `else` ~ Expr )
        rule( `if` ~ '(' ~ ExprCtx.Expr ~ ')' ~ Expr ~ Else.? )
      }
      val While = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr )
      val Try = {
        val Catch = rule( `catch` ~ Expr )
        val Finally = rule( `finally` ~ Expr )
        rule( `try` ~ Expr ~ Catch.? ~ Finally.? )
      }
      val DoWhile = rule( `do` ~ Expr ~ Semi.? ~ `while` ~ '(' ~ Expr ~ ")" )

      val For = {
        val Body = rule( '(' ~ ExprCtx.Enumerators ~ ')' | '{' ~ StatCtx.Enumerators ~ '}' )
        rule( `for` ~ Body ~ `yield`.? ~ Expr )
      }
      val Throw = rule( `throw` ~ Expr )
      val Return = rule( `return` ~ Expr.? )

      val SmallerExpr = rule( PostfixExpr ~ (`match` ~ '{' ~ CaseClauses ~ "}" | Ascription).? )
      val LambdaBody = rule( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
      rule( LambdaHead.* ~ LambdaBody )
    }

    val PostfixExpr: Rule0 = {
      val Prefixed = rule( (WL ~ anyOf("-+~!") ~ WS ~ !OpChar) ~  SimpleExpr )
      val Assign = rule( SimpleExpr ~ (`=` ~ Expr).? )
      val PrefixExpr = rule( Prefixed | Assign )

      val InfixExpr = rule( PrefixExpr ~ (NoSemis ~ Id ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr).* )
      rule( InfixExpr ~ (NotNewline ~ Id ~ LineEnd.?).? )
    }

    val SimpleExpr: Rule0 = {
      val Path = rule( (Id ~ '.').* ~ `this` ~ ('.' ~ Id).* | StableId )
      val New = rule( `new` ~ NewBody )
      val Parened = rule ( '(' ~ Exprs.? ~ ")"  )
      val SimpleExpr1 = rule( XmlExpr | New | BlockExpr | WLLiteral | Path | underscore | Parened)
      rule( SimpleExpr1 ~ ('.' ~ Id | TypeArgs | NoSemis ~ ArgList).* ~ (NoSemis  ~ underscore).?)
    }
    val Guard : Rule0 = rule( `if` ~ PostfixExpr )
  }
  val SimplePat: Rule0 = {
    val ExtractorArgs = rule( Pat.*(',') )
    val Extractor = rule( StableId ~ ('(' ~ ExtractorArgs ~ ')').? )
    val TupleEx = rule( '(' ~ ExtractorArgs.? ~ ')' )
    val Thingy = rule( underscore ~ (`:` ~ TypePat).? ~ !"*" )
    rule( XmlPattern | Thingy | WLLiteral | TupleEx | Extractor | VarId)
  }

  val BlockExpr: Rule0 = rule( '{' ~ (CaseClauses | Block) ~ `}` )

  val BlockStats: Rule0 = {
    val Prelude = rule( Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.* )
    val Tmpl = rule( Prelude ~ BlockDef )
    val BlockStat = rule( Import | Tmpl | StatCtx.Expr )
    rule( BlockStat.+(Semis) )
  }

  val Block: Rule0 = {
    val End = rule( Semis.? ~ &("}" | `case`) )
    val ResultExpr = rule{ StatCtx.Expr | LambdaHead ~ Block}
    val Body = rule( ResultExpr ~ End | BlockStats ~ (Semis ~ ResultExpr).? ~ End | End )
    rule( LambdaHead.* ~ Semis.? ~ Body )
  }

  val Patterns: Rule0 = rule( Pat.+(",") )
  val Pat: Rule0 = rule( Pat1.+('|') )
  val Pat1: Rule0 = rule( underscore ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2 )
  val Pat2: Rule0 = {
    val Pat3 = rule( `_*` | SimplePat ~ (Id ~ SimplePat).* )
    rule( VarId ~ `@` ~ Pat3 | Pat3 | VarId )
  }

  val TypePat = rule( CompoundType )

  val ArgList: Rule0 = rule( '(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")" | OneNLMax ~ BlockExpr )

  val CaseClauses: Rule0 = {
    val CaseClause: Rule0 = rule( `case` ~ Pat ~ ExprCtx.Guard.? ~ `=>` ~ Block )
    rule( CaseClause.+ )
  }
}
