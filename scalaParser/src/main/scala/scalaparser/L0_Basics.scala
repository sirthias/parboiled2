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

import org.parboiled2._

trait L0_Basics { this: Parser =>
  import CharacterClasses._

  def HexNum = rule("0x" ~ HEXDIGIT.+)

  def DecNum = rule(DIGIT.+)

  def Newline = rule(quiet('\r'.? ~ '\n'))

  def LineEnd = rule(quiet(WL ~ Newline))

  def OpChar = rule(atomic(OPCHAR | test(isMathOrOtherSymbol(cursorChar)) ~ ANY))

  def AlphaNum   = rule(ALPHANUM | GeneralAlphaNum)
  def AlphaNum$  = rule(ALPHANUM$ | GeneralAlphaNum)
  def AlphaNum$_ = rule(ALPHANUM$_ | GeneralAlphaNum)

  def GeneralLower = rule(atomic(`LOWER$_` | test(cursorChar.isLower) ~ ANY))

  /** Whitespace, including newlines. This is the default for most things.
    */
  def WL = rule(quiet((WSCHAR | Comment | Newline).*))

  /** Whitespace, excluding newlines.
    * Only really useful in e.g. {} blocks, where we want to avoid
    * capturing newlines so semicolon-inference works
    */
  def WS = rule(quiet(WSCHAR | Comment).*)

  def Semi = rule(WL ~ ';' | WS ~ Newline.+)

  def Semis = rule(Semi.+)

  def NotNewline: Rule0 = rule(&(WS ~ !Newline))

  def OneNLMax: Rule0 = rule(quiet(WS ~ Newline.? ~ CommentLine.* ~ NotNewline))

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def Comment: Rule0 = rule(BlockComment | "//" ~ (!Newline ~ ANY).*)

  private def BlockComment: Rule0 = rule("/*" ~ (BlockComment | !"*/" ~ ANY).* ~ "*/")

  private def GeneralAlphaNum = rule(test(cursorChar.isLetter | cursorChar.isDigit) ~ ANY)

  private def CommentLine = rule(quiet(WSCHAR.* ~ Comment ~ WSCHAR.* ~ Newline))

  private def isMathOrOtherSymbol(c: Char) =
    Character.getType(c) match {
      case Character.OTHER_SYMBOL | Character.MATH_SYMBOL => true
      case _                                              => false
    }
}
