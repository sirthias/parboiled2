package scalaparser

import scala.language.implicitConversions
import org.parboiled2._

trait L6_TopLevel { this: WhitespaceStringsAndChars
  with L4_Types
  with L5_Exprs =>

  import L0_Basics._
  import L1_KeywordsAndOperators._
  import L2_Identifiers._

  val CompilationUnit: Rule0 = {
    val TopPackageSeq = rule( (`package` ~ QualId ~ !(WS ~ "{")).+(Semis) )
    val Body = rule( TopPackageSeq ~ (Semis ~ TopStatSeq).? | TopStatSeq | MATCH )
    rule { Semis.? ~ Body ~ Semis.? ~ WL ~ EOI }
  }

  val TmplBody: Rule0 = {
    val Prelude = rule( (Annot ~ OneNLMax).* ~ Mod.* )
    val TmplStat = rule( Import | Prelude ~ (Def | Dcl) | StatCtx.Expr )
    val SelfType = rule( (`this` | Id | underscore) ~ (`:` ~ InfixType).? ~ `=>` )
    rule( '{' ~ SelfType.? ~ Semis.? ~ TmplStat.*(Semis) ~ `}` )
  }

  val NewBody = rule( ClsTmpl | TmplBody )

  val BlockDef = rule( Def | TmplDef )

  val ValVarDef: Rule0 = {
    val Val = rule( Pat2.+(',') ~ (`:` ~ Type).? ~ `=` ~ StatCtx.Expr )
    val Var = rule( Ids ~ `:` ~ Type ~ `=` ~ underscore | Val )
    rule( `val` ~ Val | `var` ~ Var )
  }
  val Def: Rule0 = {
    val Body = rule( `=` ~ `macro`.? ~ StatCtx.Expr | OneNLMax ~ '{' ~ Block ~ "}" )
    val FunDef = rule( `def` ~ FunSig ~ (`:` ~ Type).? ~ Body )
    rule( FunDef | TypeDef | ValVarDef | TmplDef )
  }

  val TmplDef: Rule0 = {
    val ClsDef = {
      val ClsAnnot = rule( `@` ~ SimpleType ~ ArgList )
      val Prelude = rule( NotNewline ~ ( ClsAnnot.+ ~ AccessMod.? | ClsAnnot.* ~ AccessMod) )
      val ClsArgMod = rule( (Mod.* ~ (`val` | `var`)).? )
      val ClsArg = rule( Annot.* ~ ClsArgMod ~ Id ~ `:` ~ ParamType ~ (`=` ~ ExprCtx.Expr).? )

      val Implicit = rule( OneNLMax ~ '(' ~ `implicit` ~ ClsArg.+(",") ~ ")" )
      val ClsArgs = rule( OneNLMax ~'(' ~ ClsArg.*(',') ~ ")" )
      val AllArgs = rule( ClsArgs.+ ~ Implicit.? | Implicit )
      rule( `class` ~ Id ~ TypeArgList.? ~ Prelude.? ~ AllArgs.? ~ ClsTmplOpt )
    }
    val TraitTmplOpt = {
      val TraitParents = rule( AnnotType ~ (`with` ~ AnnotType).* )
      val TraitTmpl = rule( EarlyDefs.? ~ TraitParents ~ TmplBody.? )
      rule( `extends` ~ TraitTmpl | (`extends`.? ~ TmplBody).? )
    }
    val TraitDef = rule( `trait` ~ Id ~ TypeArgList.? ~ TraitTmplOpt )
    rule( TraitDef | `case`.? ~ (ClsDef | ObjDef) )
  }

  val ObjDef: Rule0 = rule( `object` ~ Id ~ ClsTmplOpt )
  val ClsTmplOpt: Rule0 = rule( `extends` ~ ClsTmpl | (`extends`.? ~ TmplBody).? )

  val ClsTmpl: Rule0 = {
    val Constr = rule( AnnotType ~ (NotNewline ~ ArgList).* )
    val ClsParents = rule( Constr ~ (`with` ~ AnnotType).* )
    rule( EarlyDefs.? ~ ClsParents ~ TmplBody.? )
  }

  val EarlyDefs: Rule0 = {
    val EarlyDef = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ ValVarDef )
    rule( `{` ~ EarlyDef.*(Semis) ~ `}` ~ `with` )
  }

  val TopStatSeq: Rule0 = {
    val PkgObj = rule( `package` ~ ObjDef )
    val PkgBlock = rule( `package` ~ QualId ~ `{` ~ TopStatSeq.? ~ `}` )
    val Tmpl = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ TmplDef )
    val TopStat = rule( PkgBlock | PkgObj | Import | Tmpl )
    rule( TopStat.+(Semis) )
  }

  //////////////////////////// PRIVATE ///////////////////////////////////

  private val QualId = rule( WL ~ Id.+('.') )
}
