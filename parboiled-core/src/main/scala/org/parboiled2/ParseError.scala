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

import scala.annotation.tailrec
import scala.collection.immutable

case class ParseError(position: Position,
                      principalPosition: Position,
                      traces: immutable.Seq[RuleTrace]) extends RuntimeException {
  require(principalPosition.index >= position.index, "principalPosition must be > position")
  def format(parser: Parser): String = format(parser.input)
  def format(parser: Parser, formatter: ErrorFormatter): String = format(parser.input, formatter)
  def format(input: ParserInput): String = format(input, new ErrorFormatter())
  def format(input: ParserInput, formatter: ErrorFormatter): String = formatter.format(this, input)

  override def toString = s"ParseError($position, $principalPosition, <${traces.size} traces>)"
}

/**
 * Defines a position in an [[ParserInput]].
 *
 * @param index index into the input buffer (0-based)
 * @param line the text line the error occurred in (1-based)
 * @param column the text column the error occurred in (1-based)
 */
case class Position(index: Int, line: Int, column: Int)

object Position {
  def apply(index: Int, input: ParserInput): Position = {
    @tailrec def rec(ix: Int, line: Int, col: Int): Position =
      if (ix >= index) Position(index, line, col)
      else if (ix >= input.length || input.charAt(ix) != '\n') rec(ix + 1, line, col + 1)
      else rec(ix + 1, line + 1, 1)
    rec(ix = 0, line = 1, col = 1)
  }
}

sealed abstract class RuleTrace {
  import RuleTrace._

  /**
   * The number of characters before the principal error location that the rule corresponding
   * to this trace head started matching.
   */
  def offset: Int

  /**
   * Returns the [[Terminal]] trace of this trace.
   * If this trace is already a [[Terminal]] the method returns `this`.
   */
  def terminal: Terminal = {
    @tailrec def rec(current: RuleTrace): Terminal =
      current match {
        case x: NonTerminal ⇒ rec(x.tail)
        case x: Terminal    ⇒ x
      }
    rec(this)
  }

  /**
   * Folds the given function across this trace.
   */
  def fold[T](zero: T)(f: (T, RuleTrace) ⇒ T): T = {
    @tailrec def rec(current: RuleTrace, acc: T): T =
      current match {
        case x: NonTerminal ⇒ rec(x.tail, f(acc, x))
        case x: Terminal    ⇒ f(acc, x)
      }
    rec(this, zero)
  }

  /**
   * Applies the given function to all frames of the trace and returns
   * the first non-empty result or None.
   */
  def findMap[T](f: RuleTrace ⇒ Option[T]): Option[T] = {
    @tailrec def rec(current: RuleTrace): Option[T] =
      current match {
        case x: NonTerminal ⇒
          val r = f(x); if (r.isDefined) r else rec(x.tail)
        case x: Terminal ⇒ f(x)
      }
    rec(this)
  }

  /**
   * Applies the given partial function to all frames of the trace and returns
   * the first defined result or None.
   */
  def collect[T](pf: PartialFunction[RuleTrace, T]): Option[T] = findMap(pf.lift)

  /**
   * Returns the tail of the first [[RuleTrace.Atomic]] element or the [[RuleTrace.Terminal]].
   * If this is wrapped in one or more [[RuleTrace.Named]] the outermost of these is returned instead.
   */
  def firstAtomicChildOrTerminal: RuleTrace = {
    @tailrec def rec(current: RuleTrace, named: Option[RuleTrace]): RuleTrace =
      current match {
        case x: Named       ⇒ rec(x.tail, named orElse Some(x))
        case x: RuleCall    ⇒ rec(x.tail, named) // RuleCall elements allow the name to be carried over
        case x: Terminal    ⇒ named getOrElse x
        case x: Atomic      ⇒ named getOrElse x.tail
        case x: NonTerminal ⇒ rec(x.tail, None)
      }
    rec(this, None)
  }

  /**
   * Wraps this trace with a [[RuleTrace.Named]] wrapper if the given name is non-empty.
   */
  def named(name: String): RuleTrace =
    if (name.isEmpty) this else Named(name, this)
}

object RuleTrace {

  sealed trait NonTerminal extends RuleTrace {
    def tail: RuleTrace
  }
  case class Sequence(subs: Int, offset: Int, tail: RuleTrace) extends NonTerminal
  case class Cut(offset: Int, tail: RuleTrace) extends NonTerminal
  case class StringMatch(string: String, offset: Int, tail: CharMatch) extends NonTerminal
  case class IgnoreCaseString(string: String, offset: Int, tail: IgnoreCaseChar) extends NonTerminal
  case class MapMatch(map: Map[String, Any], offset: Int, tail: StringMatch) extends NonTerminal
  case class ZeroOrMore(offset: Int, tail: RuleTrace) extends NonTerminal
  case class OneOrMore(offset: Int, tail: RuleTrace) extends NonTerminal
  case class Times(min: Int, max: Int, offset: Int, tail: RuleTrace) extends NonTerminal
  case class Run(offset: Int, tail: RuleTrace) extends NonTerminal
  case class Action(offset: Int, tail: RuleTrace) extends NonTerminal
  case class RunSubParser(offset: Int, tail: RuleTrace) extends NonTerminal

  sealed abstract class DirectNonTerminal extends NonTerminal {
    def offset = tail.offset
  }
  case class Named(name: String, tail: RuleTrace) extends DirectNonTerminal
  case class FirstOf(subs: Int, tail: RuleTrace) extends DirectNonTerminal
  case class Optional(tail: RuleTrace) extends DirectNonTerminal
  case class Capture(tail: RuleTrace) extends DirectNonTerminal
  case class AndPredicate(tail: RuleTrace) extends DirectNonTerminal
  case class Atomic(tail: RuleTrace) extends DirectNonTerminal
  case class Quiet(tail: RuleTrace) extends DirectNonTerminal
  case class RuleCall(tail: RuleTrace) extends DirectNonTerminal

  sealed abstract class Terminal extends RuleTrace {
    def offset = 0
  }
  case class CharMatch(char: Char) extends Terminal
  case class IgnoreCaseChar(char: Char) extends Terminal
  case class CharPredicateMatch(predicate: CharPredicate) extends Terminal
  case class AnyOf(string: String) extends Terminal
  case class NoneOf(string: String) extends Terminal
  case class CharRange(from: Char, to: Char) extends Terminal
  case class NotPredicate(base: NotPredicate.Base, baseMatchLength: Int) extends Terminal
  case object ANY extends Terminal
  case object SemanticPredicate extends Terminal

  object NotPredicate {
    sealed trait Base
    case class Terminal(trace: RuleTrace) extends Base // `trace` can be a `Terminal` or `Named`
    case class RuleCall(target: String) extends Base
    case class Named(name: String) extends Base
    case object Anonymous extends Base
  }
}