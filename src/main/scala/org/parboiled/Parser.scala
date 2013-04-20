/*
 * Copyright (C) 2009-2013 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled

import scala.reflect.macros.Context

abstract class Parser(input: ParserInput) {
  private var cursor: Int = 0

  def rule(r: Rule): Boolean = macro Parser.ruleImpl

  implicit def charRule(c: Char) = Rule()
  implicit def stringRule(stringLiteral: String) = macro Parser.stringRuleImpl

  def ch(c: Option[Char], trgC: Char): Boolean = {
    c match {
      case Some(x) ⇒
        if (x == trgC) {
          cursor += 1
          true
        } else false
      case None ⇒ false
    }
  }

  def nextCh(): Option[Char] = {
    if (cursor < input.length) Some(input.charAt(cursor)) else None
  }
}

object Parser {
  class GrammarException(_msg: String) extends RuntimeException(_msg)

  type ParserContext = Context { type PrefixType = Parser }

  /**
   * Expands a string literal to a corresponding rule definition,
   * e.g. "abc" is expanded to `'a' ~ 'b' ~ 'c'`.
   */
  def stringRuleImpl(c: ParserContext)(stringLiteral: c.Expr[String]): c.Expr[Rule] = {
    import c.universe._
    val chars = stringLiteral match {
      case Expr(Literal(Constant("")))        ⇒ fail("String literal in rule definitions must not be empty")
      case Expr(Literal(Constant(s: String))) ⇒ s.toList
      case _                                  ⇒ fail("Strings in rule definitions have to be literals")
    }
    def wrap(char: Char) = Apply(Select(c.prefix.tree, newTermName("charRule")), List(Literal(Constant(char))))
    val tree = chars.tail.foldLeft(wrap(chars.head)) {
      case (acc, char) ⇒ Apply(Select(acc, newTermName("$tilde")), List(wrap(char)))
    }
    c.Expr[Rule](tree)
  }

  def ruleImpl(c: ParserContext)(r: c.Expr[Rule]): c.Expr[Boolean] = {
    import c.universe._

    def transform(e: Tree): c.Expr[Boolean] = {
      e match {
        case Apply(Select(This(typeName), termName), List(Literal(Constant(cnst: Char)))) ⇒
          // CharRule
          reify {
            val p = c.prefix.splice
            p.ch(p.nextCh(), c.Expr[Char](Literal(Constant(cnst))).splice)
          }
        case x ⇒
          println("ERROR: " + x)
          reify { false }
      }
    }

    //println("r.tree: " + showRaw(r.tree))
    //transform(r.tree)

    transform(r.tree)
  }

  private def fail(errorMsg: String) = throw new GrammarException(errorMsg)

}
