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

trait L5_Exprs {
  this: Parser & WhitespaceStringsAndChars & L0_Basics & L1_KeywordsAndOperators & L2_Identifiers & L3_Literals & L4_Types & L4_Xml =>

  def NewBody: Rule0
  def BlockDef: Rule0

  def Import: Rule0 = {
    def ImportExpr: Rule0 = rule(StableId ~ ('.' ~ (underscore | Selectors)).?)
    def Selectors: Rule0  = rule('{' ~ (Selector ~ ',').* ~ (Selector | underscore) ~ "}")
    def Selector: Rule0   = rule(Id ~ (`=>` ~ (Id | underscore)).?)
    rule(`import` ~ ImportExpr.+(','))
  }

  def Ascription = rule(`:` ~ (`_*` | Type | Annot.+))

  def LambdaHead: Rule0 = {
    def Binding  = rule((Id | underscore) ~ (`:` ~ Type).?)
    def Bindings = rule('(' ~ Binding.*(',') ~ ')')
    def Implicit = rule(`implicit`.? ~ Id ~ (`:` ~ InfixType).?)
    rule((Bindings | Implicit | underscore ~ Ascription.?) ~ `=>`)
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  def TypeExpr = ExprCtx.Expr

  class WsCtx(injectSemicolons: Boolean) {

    def OneSemiMax = if (injectSemicolons) OneNLMax else MATCH
    def NoSemis    = if (injectSemicolons) NotNewline else MATCH

    def Enumerators = {
      def Generator  = rule(Pat1 ~ `<-` ~ Expr ~ Guard.?)
      def Assign     = rule(Pat1 ~ `=` ~ Expr)
      def Enumerator = rule(Semis ~ Generator | optional(Semis) ~ Guard | Semis ~ Assign)
      rule(Generator ~ Enumerator.* ~ WL)
    }

    def Expr: Rule0 = {
      def If = {
        def Else = rule(Semi.? ~ `else` ~ Expr)
        rule(`if` ~ '(' ~ ExprCtx.Expr ~ ')' ~ Expr ~ Else.?)
      }
      def While = rule(`while` ~ '(' ~ Expr ~ ')' ~ Expr)
      def Try   = {
        def Catch   = rule(`catch` ~ Expr)
        def Finally = rule(`finally` ~ Expr)
        rule(`try` ~ Expr ~ Catch.? ~ Finally.?)
      }
      def DoWhile = rule(`do` ~ Expr ~ Semi.? ~ `while` ~ '(' ~ Expr ~ ")")

      def For = {
        def Body = rule('(' ~ ExprCtx.Enumerators ~ ')' | '{' ~ StatCtx.Enumerators ~ '}')
        rule(`for` ~ Body ~ `yield`.? ~ Expr)
      }
      def Throw  = rule(`throw` ~ Expr)
      def Return = rule(`return` ~ Expr.?)

      def SmallerExpr = rule(PostfixExpr ~ (`match` ~ '{' ~ CaseClauses ~ "}" | Ascription).?)
      def LambdaBody  = rule(If | While | Try | DoWhile | For | Throw | Return | SmallerExpr)
      rule(LambdaHead.* ~ LambdaBody)
    }

    def PostfixExpr: Rule0 = {
      def Prefixed   = rule((WL ~ anyOf("-+~!") ~ WS ~ !OpChar) ~ SimpleExpr)
      def Assign     = rule(SimpleExpr ~ (`=` ~ Expr).?)
      def PrefixExpr = rule(Prefixed | Assign)

      def InfixExpr = rule(PrefixExpr ~ (NoSemis ~ Id ~ TypeArgs.? ~ OneSemiMax ~ PrefixExpr).*)
      rule(InfixExpr ~ (NotNewline ~ Id ~ LineEnd.?).?)
    }

    def SimpleExpr: Rule0 = {
      def Path        = rule((Id ~ '.').* ~ `this` ~ ('.' ~ Id).* | StableId)
      def New         = rule(`new` ~ NewBody)
      def Parened     = rule('(' ~ Exprs.? ~ ")")
      def SimpleExpr1 = rule(XmlExpr | New | BlockExpr | WLLiteral | Path | underscore | Parened)
      rule(SimpleExpr1 ~ ('.' ~ Id | TypeArgs | NoSemis ~ ArgList).* ~ (NoSemis ~ underscore).?)
    }
    def Guard: Rule0 = rule(`if` ~ PostfixExpr)
  }

  def SimplePat: Rule0 = {
    def ExtractorArgs = rule(Pat.*(','))
    def Extractor     = rule(StableId ~ ('(' ~ ExtractorArgs ~ ')').?)
    def TupleEx       = rule('(' ~ ExtractorArgs.? ~ ')')
    def Thingy        = rule(underscore ~ (`:` ~ TypePat).? ~ !"*")
    rule(XmlPattern | Thingy | WLLiteral | TupleEx | Extractor | VarId)
  }

  def BlockExpr: Rule0 = rule('{' ~ (CaseClauses | Block) ~ `}`)

  def BlockStats: Rule0 = {
    def Prelude   = rule(Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.*)
    def Tmpl      = rule(Prelude ~ BlockDef)
    def BlockStat = rule(Import | Tmpl | StatCtx.Expr)
    rule(BlockStat.+(Semis))
  }

  def Block: Rule0 = {
    def End        = rule(Semis.? ~ &("}" | `case`))
    def ResultExpr = rule(StatCtx.Expr | LambdaHead ~ Block)
    def Body       = rule(ResultExpr ~ End | BlockStats ~ (Semis ~ ResultExpr).? ~ End | End)
    rule(LambdaHead.* ~ Semis.? ~ Body)
  }

  def Patterns: Rule0 = rule(Pat.+(","))
  def Pat: Rule0      = rule(Pat1.+('|'))
  def Pat1: Rule0     = rule(underscore ~ `:` ~ TypePat | VarId ~ `:` ~ TypePat | Pat2)

  def Pat2: Rule0 = {
    def Pat3 = rule(`_*` | SimplePat ~ (Id ~ SimplePat).*)
    rule(VarId ~ `@` ~ Pat3 | Pat3 | VarId)
  }

  def TypePat = rule(CompoundType)

  def ArgList: Rule0 = rule('(' ~ (Exprs ~ (`:` ~ `_*`).?).? ~ ")" | OneNLMax ~ BlockExpr)

  def CaseClauses: Rule0 = {
    def CaseClause: Rule0 = rule(`case` ~ Pat ~ ExprCtx.Guard.? ~ `=>` ~ Block)
    rule(CaseClause.+)
  }
}
