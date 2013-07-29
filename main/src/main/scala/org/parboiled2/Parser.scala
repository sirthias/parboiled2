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

package org.parboiled2

import scala.reflect.macros.Context
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

case class ParseError(position: InputPosition, errorRules: Seq[RuleStack])
case class InputPosition(line: Int, column: Int)
case class RuleStack(frames: Seq[RuleFrame])

sealed trait RuleFrame
case class SequenceFrame() extends RuleFrame
case class CharacterClassFrame(from: Char, to: Char) extends RuleFrame
case class FirstOfFrame() extends RuleFrame
case class StringFrame(string: String) extends RuleFrame
case class CharFrame(char: Char) extends RuleFrame
case class RuleCallFrame(name: String) extends RuleFrame
case class OptionalFrame() extends RuleFrame
case class ZeroOrMoreFrame() extends RuleFrame
case class OneOrMoreFrame() extends RuleFrame
case class AndPredicateFrame() extends RuleFrame
case class NotPredicateFrame() extends RuleFrame

abstract class Parser {
  def input: ParserInput

  var collecting = false

  val currentCallStack = Stack[RuleFrame]()
  val expectedRules = ArrayBuffer[RuleStack]()

  type Mark = Int
  private var _mark: Mark = 0
  private var _errorMark: Mark = 0

  def rule(r: Rule): Rule = macro Parser.ruleImpl

  def run(rule: ⇒ Rule): Either[ParseError, Unit] = {
    if (rule.matched) Right(Unit)
    else {
      collecting = true
      val _ = rule.matched
      collecting = false
      if (errorMark == 0) {
        Left(ParseError(InputPosition(0, 0), expectedRules))
      } else {
        val prefixString = input.sliceString(0, errorMark)
        val line = prefixString.count(_ == '\n')
        val prevEol = prefixString.lastIndexOf('\n', errorMark - 1)
        val column =
          if (prevEol != -1) errorMark - prevEol
          else errorMark
        Left(ParseError(InputPosition(line, column), expectedRules))
      }
    }
  }

  implicit def ch(c: Char) = Rule()
  implicit def str(s: String) = Rule()

  def zeroOrMore(r: Rule) = Rule()
  def oneOrMore(r: Rule) = Rule()
  def optional(r: Rule) = Rule()
  def &(r: Rule): Rule = Rule()

  def nextChar(): Char =
    if (_mark < input.length) {
      val nextCh = input.charAt(_mark)
      _mark += 1
      nextCh
    } else EOI

  def mark: Mark = _mark
  def errorMark: Mark = _errorMark
  def reset(mark: Mark): Unit = {
    if (!collecting && mark > _errorMark)
      _errorMark = mark
    _mark = mark
  }

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
