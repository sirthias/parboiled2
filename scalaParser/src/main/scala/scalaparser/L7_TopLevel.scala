package scalaparser

import scala.language.implicitConversions
import org.parboiled2._

trait L7_TopLevel { this: Parser with WhitespaceStringsAndChars
  with L0_Basics
  with L1_KeywordsAndOperators
  with L4_Core
  with L5_Types
  with L6_Exprs =>

  def CompilationUnit: Rule0 = {
    def TopPackageSeq = rule( (`package` ~ QualId ~ !(WS ~ "{")).+(Semis) )
    def Body = rule( TopPackageSeq ~ (Semis ~ TopStatSeq).? | TopStatSeq | MATCH )
    rule { Semis.? ~ Body ~ Semis.? ~ WL ~ EOI }
  }

  def TmplBody: Rule0 = {
    def Prelude = rule( (Annot ~ OneNLMax).* ~ Mod.* )
    def TmplStat = rule( Import | Prelude ~ (Def | Dcl) | StatCtx.Expr )
    def SelfType = rule( (`this` | WLId | `_`) ~ (`:` ~ InfixType).? ~ `=>` )
    rule( '{' ~ SelfType.? ~ Semis.? ~ TmplStat.*(Semis) ~ `}` )
  }

  def NewBody = rule( ClsTmpl | TmplBody )

  def BlockDef = rule( Def | TmplDef )

  def ValVarDef: Rule0 = {
    def Val = rule( Pat2.+(',') ~ (`:` ~ Type).? ~ `=` ~ StatCtx.Expr )
    def Var = rule( WLIds ~ `:` ~ Type ~ `=` ~ `_` | Val )
    rule( `val` ~ Val | `var` ~ Var )
  }
  def Def: Rule0 = {
    def Body = rule( `=` ~ `macro`.? ~ StatCtx.Expr | OneNLMax ~ '{' ~ Block ~ "}" )
    def FunDef = rule( `def` ~ FunSig ~ (`:` ~ Type).? ~ Body )
    rule( FunDef | TypeDef | ValVarDef | TmplDef )
  }

  def TmplDef: Rule0 = {
    def ClsDef = {
      def ClsAnnot = rule( `@` ~ SimpleType ~ ArgList )
      def Prelude = rule( NotNewline ~ ( ClsAnnot.+ ~ AccessMod.? | ClsAnnot.* ~ AccessMod) )
      def ClsArgMod = rule( (Mod.* ~ (`val` | `var`)).? )
      def ClsArg = rule( Annot.* ~ ClsArgMod ~ WLId ~ `:` ~ ParamType ~ (`=` ~ ExprCtx.Expr).? )

      def Implicit = rule( OneNLMax ~ '(' ~ `implicit` ~ ClsArg.+(",") ~ ")" )
      def ClsArgs = rule( OneNLMax ~'(' ~ ClsArg.*(',') ~ ")" )
      def AllArgs = rule( ClsArgs.+ ~ Implicit.? | Implicit )
      rule( `class` ~ WLId ~ TypeArgList.? ~ Prelude.? ~ AllArgs.? ~ ClsTmplOpt )
    }
    def TraitTmplOpt = {
      def TraitParents = rule( AnnotType ~ (`with` ~ AnnotType).* )
      def TraitTmpl = rule( EarlyDefs.? ~ TraitParents ~ TmplBody.? )
      rule( `extends` ~ TraitTmpl | (`extends`.? ~ TmplBody).? )
    }
    def TraitDef = rule( `trait` ~ WLId ~ TypeArgList.? ~ TraitTmplOpt )
    rule( TraitDef | `case`.? ~ (ClsDef | ObjDef) )
  }

  def ObjDef: Rule0 = rule( `object` ~ WLId ~ ClsTmplOpt )
  def ClsTmplOpt: Rule0 = rule( `extends` ~ ClsTmpl | (`extends`.? ~ TmplBody).? )

  def ClsTmpl: Rule0 = {
    def Constr = rule( AnnotType ~ (NotNewline ~ ArgList).* )
    def ClsParents = rule( Constr ~ (`with` ~ AnnotType).* )
    rule( EarlyDefs.? ~ ClsParents ~ TmplBody.? )
  }

  def EarlyDefs: Rule0 = {
    def EarlyDef = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ ValVarDef )
    rule( `{` ~ EarlyDef.*(Semis) ~ `}` ~ `with` )
  }

  def TopStatSeq: Rule0 = {
    def PkgObj = rule( `package` ~ ObjDef )
    def PkgBlock = rule( `package` ~ QualId ~ `{` ~ TopStatSeq.? ~ `}` )
    def Tmpl = rule( (Annot ~ OneNLMax).* ~ Mod.* ~ TmplDef )
    def TopStat = rule( PkgBlock | PkgObj | Import | Tmpl )
    rule( TopStat.+(Semis) )
  }
}
