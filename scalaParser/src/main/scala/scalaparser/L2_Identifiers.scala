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

trait L2_Identifiers { self: Parser & L0_Basics & L1_KeywordsAndOperators =>

  def VarId                    = rule(WL ~ !Keyword ~ GeneralLower ~ IdRestWithDollar)
  def RawIdNoBackticks         = rule(!Keyword ~ AlphaNum$_ ~ IdRestWithDollar | Operator)
  def RawIdNoDollarNoBackticks = rule(!Keyword ~ AlphaNum$_ ~ IdRest | Operator)
  def RawId                    = rule(RawIdNoBackticks | '`' ~ (!'`' ~ ANY).+ ~ '`') // FIXME: are newlines in backticks allowed?
  def Id                       = rule(WL ~ RawId)
  def Ids                      = rule(Id.+(WL ~ ','))

  def StableId: Rule0 = {
    def ClassQualifier  = rule(WL ~ '[' ~ Id ~ WL ~ ']')
    def `.`             = rule(WL ~ '.')
    def ThisOrSuper     = rule(`this` | `super` ~ ClassQualifier.?)
    def ThisOrSuperTail = rule(ThisOrSuper ~ (`.` ~ Id).*)
    rule(Id.+(`.`) ~ (`.` ~ ThisOrSuperTail).? | ThisOrSuperTail)
  }

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def IdRest           = rule((Underscores ~ AlphaNum.+).* ~ OpSuffix)
  private def IdRestWithDollar = rule((Underscores ~ AlphaNum$.+).* ~ OpSuffix)
  private def Underscores      = rule(ch('_').*)
  private def OpSuffix         = rule((ch('_').+ ~ OpChar.*).?)
}
