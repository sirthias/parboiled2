package scalaparser

import org.parboiled2._

trait L4_Types { this: WhitespaceStringsAndChars with L3_Literals =>
  import L0_Basics._
  import L1_KeywordsAndOperators._
  import L2_Identifiers._

  val TypeExpr: Rule0

  val Mod: Rule0 = rule( LocalMod | AccessMod | `override` )
  val LocalMod: Rule0 = rule( `abstract` | `final` | `sealed` | `implicit` | `lazy` )
  val AccessMod: Rule0 = {
    val AccessQualifier = rule( '[' ~ (`this` | Id) ~ ']' )
    rule( (`private` | `protected`) ~ AccessQualifier.? )
  }

  val Dcl: Rule0 = {
    val VarDcl = rule( `var` ~ Ids ~ `:` ~ Type )
    val FunDcl = rule( `def` ~ FunSig ~ (`:` ~ Type).? )
    rule( ValDcl | VarDcl | FunDcl | TypeDcl )
  }

  val Type: Rule0 = {
    val FunctionArgTypes = rule('(' ~ ParamType.+(',').? ~ ')' )
    val ArrowType = rule( FunctionArgTypes ~ `=>` ~ Type )
    val ExistentialClause = rule( `forSome` ~ `{` ~ (TypeDcl | ValDcl).+(Semis) ~ `}` )
    val PostfixType = rule( InfixType ~ (`=>` ~ Type | ExistentialClause.?) )
    val Unbounded = rule( underscore | ArrowType | PostfixType )
    rule( Unbounded ~ TypeBounds )
  }

  val InfixType = rule( CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).* )

  val CompoundType = {
    val RefineStat = rule( TypeDef | Dcl  )
    val Refinement = rule( OneNLMax ~ `{` ~ RefineStat.*(Semis) ~ `}` )
    rule( AnnotType.+(`with`) ~ Refinement.? | Refinement )
  }
  val AnnotType = rule(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).+).? )

  val SimpleType: Rule0 = {
    val BasicType = rule( '(' ~ Types ~ ')'  | StableId ~ '.' ~ `type` | StableId )
    rule( BasicType ~ (TypeArgs | `#` ~ Id).* )
  }

  val TypeArgs = rule( '[' ~ Types ~ "]" )
  val Types = rule( Type.+(',') )

  val ValDcl: Rule0 = rule( `val` ~ Ids ~ `:` ~ Type )
  val TypeDcl: Rule0 = rule( `type` ~ Id ~ TypeArgList.? ~ TypeBounds )

  val FunSig: Rule0 = {
    val FunArg = rule( Annot.* ~ Id ~ (`:` ~ ParamType).? ~ (`=` ~ TypeExpr).? )
    val FunTypeArgs = rule( '[' ~ (Annot.* ~ TypeArg).+(',') ~ ']' )
    val Args = rule( FunArg.+(',') )
    val FunArgs = rule( OneNLMax ~ '(' ~ Args.? ~ ')' )
    val FunAllArgs = rule( FunArgs.* ~ (OneNLMax ~ '(' ~ `implicit` ~ Args ~ ')').? )
    rule( (Id | `this`) ~ FunTypeArgs.? ~ FunAllArgs )
  }
  val ParamType = rule( `=>` ~ Type | Type ~ "*" | Type )

  val TypeBounds: Rule0 = rule( (`>:` ~ Type).? ~ (`<:` ~ Type).? )
  val TypeArg: Rule0 = {
    val CtxBounds = rule((`<%` ~ Type).* ~ (`:` ~ Type).*)
    rule((Id | underscore) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  val Annot: Rule0 = rule( `@` ~ SimpleType ~  ('(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")").*  )

  val TypeArgList: Rule0 = {
    val Variant: Rule0 = rule( Annot.* ~ (WL ~ anyOf("+-")).? ~ TypeArg )
    rule( '[' ~ Variant.*(',') ~ ']' )
  }
  val Exprs: Rule0 = rule( TypeExpr.+(',') )
  val TypeDef: Rule0 = rule( `type` ~ Id ~ TypeArgList.? ~ `=` ~ Type )
}
