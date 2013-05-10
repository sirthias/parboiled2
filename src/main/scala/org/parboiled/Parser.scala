/*
 * Copyright (C) 2009-2013 Mathias Doenitz, Alexander Myltsev
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

import org.parboiled.optree._

abstract class Parser(input: ParserInput) {
  private var _cursor: Int = 0
  val EOI: Char = '\uFFFF'

  def rule(r: Rule): Boolean = macro Parser.ruleImpl

  implicit def charRule(c: Char) = Rule()
  implicit def stringRule(stringLiteral: String) = Rule()

  def nextChar(): Char =
    if (_cursor < input.length) { val nextCh = input.charAt(_cursor); _cursor += 1; nextCh }
    else EOI

  def cursor_=(v: Int) = _cursor = v
  def cursor = _cursor
}

object Parser {
  class GrammarException(_msg: String) extends RuntimeException(_msg)

  type ParserContext = Context { type PrefixType = Parser }

  def transformToOpTree(c: ParserContext)(e: c.Tree): Expression = {
    import c.universe._

    e match {
      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if argTermName.decoded == "EOI" ⇒
        EOI

      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if termName.decoded == "stringRule" ⇒
        fail("Strings in rule definitions have to be literals")

      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: String)))) ⇒
        LiteralString(x)

      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: Char)))) ⇒
        LiteralChar(x)

      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$tilde\")" ⇒
        Sequence(transformToOpTree(c)(a), transformToOpTree(c)(arg))

      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$bar$bar\")" ⇒
        FirstOf(transformToOpTree(c)(a), transformToOpTree(c)(arg))

      case x ⇒
        fail("transformToOpTree, unexpected: " + showRaw(x))
    }
  }

  def transformToAst(c: ParserContext)(e: Expression): c.Expr[Boolean] = {
    import c.universe._

    e match {
      case EOI ⇒ reify {
        val p = c.prefix.splice
        p.nextChar == p.EOI
      }

      case LiteralString(targetString: String) ⇒ reify {
        val p = c.prefix.splice
        val cursorPos = p.cursor
        val ts = c.literal(targetString).splice
        if (ts forall (_ == p.nextChar)) true
        else { p.cursor = cursorPos; false }
      }

      case LiteralChar(targetChar: Char) ⇒ reify {
        val p = c.prefix.splice
        val tc = c.literal(targetChar).splice
        p.nextChar == tc
      }

      case Sequence(lhs: Expression, rhs: Expression) ⇒ reify {
        val p = c.prefix.splice
        val cursorPos = p.cursor
        if (transformToAst(c)(lhs).splice) transformToAst(c)(rhs).splice
        else { p.cursor = cursorPos; false }
      }

      case FirstOf(lhs: Expression, rhs: Expression) ⇒ reify {
        val p = c.prefix.splice
        val cursorPos = p.cursor
        if (transformToAst(c)(lhs).splice) true
        else { p.cursor = cursorPos; transformToAst(c)(rhs).splice }
      }

      case x ⇒
        fail("transformToAst, unexpected: " + x)
    }
  }

  def ruleImpl(c: ParserContext)(r: c.Expr[Rule]): c.Expr[Boolean] = {
    val opTree = transformToOpTree(c)(r.tree)
    transformToAst(c)(opTree)
  }

  private def fail(errorMsg: String) = throw new GrammarException(errorMsg)
}