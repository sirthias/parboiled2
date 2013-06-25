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

import org.parboiled.optree._
import scala.reflect.macros.Context

abstract class Parser {
  def input: ParserInput

  private var _cursor: Int = 0

  def rule(r: Rule): Rule = macro Parser.ruleImpl

  implicit def charRule(c: Char) = Rule()
  implicit def stringRule(stringLiteral: String) = Rule()

  def zeroOrMore(r: Rule) = Rule()
  def oneOrMore(r: Rule) = Rule()
  def optional(r: Rule) = Rule()

  def nextChar(): Char =
    if (_cursor < input.length) {
      val nextCh = input.charAt(_cursor)
      _cursor += 1
      nextCh
    } else EOI

  type Mark = Int
  def mark: Mark = _cursor
  def reset(mark: Mark): Unit = _cursor = mark

  def EOI = Parser.EOI
}

object Parser {
  val EOI: Char = '\uFFFF'

  type ParserContext = Context { type PrefixType = Parser }

  def ruleImpl(ctx: ParserContext)(r: ctx.Expr[Rule]): ctx.Expr[Rule] = {
    val opTreeCtx = new OpTreeContext[ctx.type] { val c: ctx.type = ctx }
    val opTree = opTreeCtx.OpTree(r.tree)
    opTree.render()
  }
}