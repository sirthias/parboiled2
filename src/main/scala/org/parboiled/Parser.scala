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
  type ParserContext = Context { type PrefixType = Parser }

  def ruleImpl(ctx: ParserContext)(r: ctx.Expr[Rule]): ctx.Expr[Boolean] = {
    val opTreeCtx = new OpTreeContext[ctx.type] { val c: ctx.type = ctx }
    import opTreeCtx._
    val opTree = OpTree(r.tree)
    opTree.render
  }
}