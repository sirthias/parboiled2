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

case class ParseError(line: Int, column: Int, actualChar: Char, expectedRules: Seq[String], input: ParserInput)

abstract class Parser {
  def input: ParserInput

  var collecting = false

  private val _expectedValues = ArrayBuffer[String]()
  def expectedValues = _expectedValues

  type ErrorMarker = Int
  def addError(expectedValue: String) = {
    _expectedValues += expectedValue
  }

  def errorMarker(): ErrorMarker =
    _expectedValues.size

  def resetErrorMarker(errorMarker: ErrorMarker): Unit =
    _expectedValues.reduceToSize(errorMarker)

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
      val errMark = errorMark()
      val actualChar =
        if (errMark == input.length) EOI
        else input.charAt(errMark)

      if (errMark == 0) {
        Left(ParseError(0, 0, actualChar, expectedValues, input))
      } else {
        val prefixString = input.sliceString(0, errMark)
        val line = prefixString.count(_ == '\n')
        val prevEol = prefixString.lastIndexOf('\n', errMark - 1)
        val column =
          if (prevEol != -1) errMark - prevEol
          else errMark
        Left(ParseError(line, column, actualChar, expectedValues, input))
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

  def mark(): Mark = _mark
  def errorMark(): Mark = _errorMark
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
