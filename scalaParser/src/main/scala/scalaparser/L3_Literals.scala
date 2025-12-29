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

trait L3_Literals { self: Parser with L0_Basics with L1_KeywordsAndOperators with L2_Identifiers =>
  import CharacterClasses.*

  def Block: Rule0

  def Literal = rule(("-".? ~ (Float | Int)) | Bool | Char | String | Symbol | Null)

  def Float = {
    def Exp      = rule(`Ee` ~ `+-`.? ~ DecNum)
    def Decimals = rule('.' ~ DIGIT.+ ~ Exp.? ~ `FfDd`.?)
    rule(Decimals | DIGIT.+ ~ (Decimals | Exp ~ `FfDd`.? | `FfDd`))
  }

  def Int = rule((HexNum | DecNum) ~ `Ll`.?)

  def Bool = rule(True | False)

  def Char = rule("'" ~ (UnicodeEscape | EscapedChars | !'\\' ~ test(isPrintableChar(cursorChar)) ~ ANY) ~ "'")

  def Symbol = rule('\'' ~ (RawIdNoBackticks | Keyword)) // symbols can take on the same values as keywords!

  def String = {
    def TripleTail = rule('"' ~ '"' ~ ch('"').+)
    def Inter      = rule('$' ~ (RawIdNoDollarNoBackticks | '{' ~ Block ~ WL ~ '}' | '$'))
    def Raw        = rule('"'.? ~ '"'.? ~ !'\"' ~ ANY)
    def Simple     = rule('\\' ~ DQBS | !DQLF ~ ANY)
    rule(
      RawId ~ '"' ~ ('"' ~ '"' ~ (Inter | Raw).* ~ TripleTail | (Inter | Simple).* ~ '"')
        | '"' ~ ('"' ~ '"' ~ Raw.* ~ TripleTail | Simple.* ~ '"')
    )
  }

  def WLLiteral = rule(WL ~ Literal)

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def UnicodeEscape = rule("\\u" ~ HEXDIGIT ~ HEXDIGIT ~ HEXDIGIT ~ HEXDIGIT)

  private def EscapedChars = rule('\\' ~ ESCAPEE)

  private def isPrintableChar(c: Char): Boolean =
    !Character.isISOControl(c) &&
      !Character.isSurrogate(c) && {
        val block = Character.UnicodeBlock.of(c); block != null && block != Character.UnicodeBlock.SPECIALS
      }
}
