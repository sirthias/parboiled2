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

import scala.annotation.switch

trait L1_KeywordsAndOperators { this: Parser & L0_Basics =>
  import CharacterClasses.*

  def Operator = rule(!SymbolicKeyword ~ OpChar.+)

  def Keyword = rule(AlphaKeyword | SymbolicKeyword)

  def underscore  = Key("_")
  def `abstract`  = Key("abstract")
  def `case`      = Key("case")
  def `catch`     = Key("catch")
  def `class`     = Key("class")
  def `def`       = Key("def")
  def `do`        = Key("do")
  def `else`      = Key("else")
  def `extends`   = Key("extends")
  def `final`     = Key("final")
  def `finally`   = Key("finally")
  def `for`       = Key("for")
  def `forSome`   = Key("forSome")
  def `if`        = Key("if")
  def `implicit`  = Key("implicit")
  def `import`    = Key("import")
  def `lazy`      = Key("lazy")
  def `macro`     = Key("macro")
  def `match`     = Key("match")
  def `new`       = Key("new")
  def `object`    = Key("object")
  def `override`  = Key("override")
  def `package`   = Key("package")
  def `private`   = Key("private")
  def `protected` = Key("protected")
  def `return`    = Key("return")
  def `sealed`    = Key("sealed")
  def `super`     = Key("super")
  def `this`      = Key("this")
  def `throw`     = Key("throw")
  def `trait`     = Key("trait")
  def `try`       = Key("try")
  def `type`      = Key("type")
  def `val`       = Key("val")
  def `var`       = Key("var")
  def `while`     = Key("while")
  def `with`      = Key("with")
  def `yield`     = Key("yield")

  def `<%` = SymbolicKey("<%")
  def `>:` = SymbolicKey(">:")
  def `<:` = SymbolicKey("<:")
  def `=>` = rule(SymbolicKey("=>") | SymbolicKey('⇒'))
  def `<-` = rule(SymbolicKey("<-") | SymbolicKey('←'))
  def `:`  = SymbolicKey(':')
  def `=`  = SymbolicKey('=')
  def `@`  = SymbolicKey('@')
  def `#`  = SymbolicKey("#")

  def Null  = RawKey("null")
  def True  = RawKey("true")
  def False = RawKey("false")

  // keyword-like patterns (not really keywords though)
  def `_*` = rule(underscore ~ WL ~ "*")
  def `}`  = rule(Semis.? ~ WL ~ '}')
  def `{`  = rule(WL ~ '{' ~ Semis.?)

  //////////////////////////// PRIVATE ///////////////////////////////////

  private def Key(s: String)         = rule(WL ~ RawKey(s))
  private def RawKey(s: String)      = rule(s ~ !AlphaNum$_)
  private def SymbolicKey(c: Char)   = rule(WL ~ c ~ !OpChar)
  private def SymbolicKey(s: String) = rule(WL ~ s ~ !OpChar)

  private def SymbolicKeyword = rule(("=>" | KEYCHAR | '<' ~ KEYCHAR2 | ">:") ~ !OpChar)

  private def AlphaKeyword = rule {
    run {
      // TODO: simplify when https://github.com/sirthias/parboiled2/issues/115 is done
      (cursorChar: @switch) match {
        case 'a' => str("abstract")
        case 'c' => "case" | "catch" | "class"
        case 'd' => "def" | "do"
        case 'e' => "else" | "extends"
        case 'f' => "false" | "final" ~ "ly".? | "forSome" | "for"
        case 'i' => "if" | "implicit" | "import"
        case 'l' => str("lazy")
        case 'm' => str("match")
        case 'n' => "new" | "null"
        case 'o' => "object" | "override"
        case 'p' => "package" | "private" | "protected"
        case 'r' => str("return")
        case 's' => "sealed" | "super"
        case 't' => "this" | "throw" | "trait" | "try" | "true" | "type"
        case 'v' => "val" | "var"
        case 'w' => "while" | "with"
        case 'y' => str("yield")
        case '_' => ANY
        case _   => MISMATCH
      }
    } ~ !AlphaNum$_
  }
}
