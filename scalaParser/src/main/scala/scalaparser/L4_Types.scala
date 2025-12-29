/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalaparser

import org.parboiled2.*

trait L4_Types {
  this: Parser
    with WhitespaceStringsAndChars with L0_Basics with L1_KeywordsAndOperators with L2_Identifiers with L3_Literals =>

  def TypeExpr: Rule0

  def Mod: Rule0      = rule(LocalMod | AccessMod | `override`)
  def LocalMod: Rule0 = rule(`abstract` | `final` | `sealed` | `implicit` | `lazy`)

  def AccessMod: Rule0 = {
    def AccessQualifier = rule('[' ~ (`this` | Id) ~ ']')
    rule((`private` | `protected`) ~ AccessQualifier.?)
  }

  def Dcl: Rule0 = {
    def VarDcl = rule(`var` ~ Ids ~ `:` ~ Type)
    def FunDcl = rule(`def` ~ FunSig ~ (`:` ~ Type).?)
    rule(ValDcl | VarDcl | FunDcl | TypeDcl)
  }

  def Type: Rule0 = {
    def FunctionArgTypes  = rule('(' ~ ParamType.+(',').? ~ ')')
    def ArrowType         = rule(FunctionArgTypes ~ `=>` ~ Type)
    def ExistentialClause = rule(`forSome` ~ `{` ~ (TypeDcl | ValDcl).+(Semis) ~ `}`)
    def PostfixType       = rule(InfixType ~ (`=>` ~ Type | ExistentialClause.?))
    def Unbounded         = rule(underscore | ArrowType | PostfixType)
    rule(Unbounded ~ TypeBounds)
  }

  def InfixType = rule(CompoundType ~ (NotNewline ~ Id ~ OneNLMax ~ CompoundType).*)

  def CompoundType = {
    def RefineStat = rule(TypeDef | Dcl)
    def Refinement = rule(OneNLMax ~ `{` ~ RefineStat.*(Semis) ~ `}`)
    rule(AnnotType.+(`with`) ~ Refinement.? | Refinement)
  }
  def AnnotType = rule(SimpleType ~ (NotNewline ~ (NotNewline ~ Annot).+).?)

  def SimpleType: Rule0 = {
    def BasicType = rule('(' ~ Types ~ ')' | StableId ~ '.' ~ `type` | StableId)
    rule(BasicType ~ (TypeArgs | `#` ~ Id).*)
  }

  def TypeArgs = rule('[' ~ Types ~ "]")
  def Types    = rule(Type.+(','))

  def ValDcl: Rule0  = rule(`val` ~ Ids ~ `:` ~ Type)
  def TypeDcl: Rule0 = rule(`type` ~ Id ~ TypeArgList.? ~ TypeBounds)

  def FunSig: Rule0 = {
    def FunTypeArgs = rule('[' ~ (Annot.* ~ TypeArg).+(',') ~ ']')
    def FunAllArgs  = rule(FunArgs.* ~ (OneNLMax ~ '(' ~ `implicit` ~ Args ~ ')').?)
    def FunArgs     = rule(OneNLMax ~ '(' ~ Args.? ~ ')')
    def FunArg      = rule(Annot.* ~ Id ~ (`:` ~ ParamType).? ~ (`=` ~ TypeExpr).?)
    def Args        = rule(FunArg.+(','))
    rule((Id | `this`) ~ FunTypeArgs.? ~ FunAllArgs)
  }
  def ParamType = rule(`=>` ~ Type | Type ~ "*" | Type)

  def TypeBounds: Rule0 = rule((`>:` ~ Type).? ~ (`<:` ~ Type).?)

  def TypeArg: Rule0 = {
    def CtxBounds = rule((`<%` ~ Type).* ~ (`:` ~ Type).*)
    rule((Id | underscore) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  def Annot: Rule0 = rule(`@` ~ SimpleType ~ ('(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")").*)

  def TypeArgList: Rule0 = {
    def Variant: Rule0 = rule(Annot.* ~ (WL ~ anyOf("+-")).? ~ TypeArg)
    rule('[' ~ Variant.*(',') ~ ']')
  }
  def Exprs: Rule0   = rule(TypeExpr.+(','))
  def TypeDef: Rule0 = rule(`type` ~ Id ~ TypeArgList.? ~ `=` ~ Type)
}
