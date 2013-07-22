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

import org.parboiled2.optree._
import scala.reflect.macros.Context
import scala.collection.mutable.ArrayBuffer

case class ParserError(mark: Parser#Mark, actual: Char, expected: Seq[String])

abstract class Parser {
  def input: ParserInput

  def debug = false

  var tab = 0
  def debug(str: String, delim: String): Unit = {
    if (debug) {
      println(s"${"\t" * tab}$delim$str")
    }
  }

  private var _collecting = false
  def collecting: Boolean = _collecting
  def collecting_=(v: Boolean) { _collecting = v }

  private val _expectedValues = ArrayBuffer[String]()
  def error(): Option[ParserError] = {
    if (_errorMark.cursor == input.length) None
    else Some(ParserError(errorMark(), input.charAt(_errorMark.cursor), _expectedValues))
  }

  type ErrorMarker = Int
  def addError(expectedValue: String) = {
    debug(s"error added: $expectedValue", "!!!")
    _expectedValues += expectedValue
  }

  def errorMarker(): ErrorMarker =
    _expectedValues.size

  def resetErrorMarker(errorMarker: ErrorMarker): Unit =
    _expectedValues.reduceToSize(errorMarker)

  private val _mark: Mark = new Mark(0, 0, 0)
  private val _errorMark: Mark = new Mark(0, 0, 0)

  def rule(r: Rule): Rule = macro Parser.ruleImpl

  implicit def ch(c: Char) = Rule()
  implicit def str(s: String) = Rule()

  def zeroOrMore(r: Rule) = Rule()
  def oneOrMore(r: Rule) = Rule()
  def optional(r: Rule) = Rule()
  def &(r: Rule): Rule = Rule()

  def nextChar(): Char =
    if (_mark.cursor < input.length) {
      if (input.charAt(_mark.cursor) == '\n') {
        _mark.line += 1
        _mark.column = 0
      }
      val nextCh = input.charAt(_mark.cursor)
      _mark.cursor += 1
      _mark.column += 1
      nextCh
    } else EOI

  case class Mark(var line: Int, var column: Int, var cursor: Int) {
    def >(that: Mark): Boolean = this.cursor > that.cursor
    def ==(that: Mark): Boolean = this.cursor == that.cursor
    def <(that: Mark) = !(this == that && this > that)
    def <~(that: Mark) = {
      this.cursor = that.cursor
      this.line = that.line
      this.column = that.column
    }
  }
  def mark(): Mark = Mark(_mark.line, _mark.column, _mark.cursor)
  def errorMark(): Mark = Mark(_errorMark.line, _errorMark.column, _errorMark.cursor)
  def reset(mark: Mark): Unit = {
    if (!_collecting && mark > _errorMark)
      _errorMark <~ mark
    _mark <~ mark
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
