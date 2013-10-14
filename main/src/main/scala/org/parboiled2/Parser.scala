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
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import shapeless._

abstract class Parser extends RuleDSL {
  import Parser._

  def input: ParserInput

  // the index of the current input char
  private[this] var cursor: Int = _

  // the highest cursor value we have seen in the current rule run
  private[this] var errorIndex: Int = _

  // the number if times we have already seen a character mismatch at the error index
  private[this] var mismatchesAtErrorIndex: Int = _

  // the index of the RuleStack we are currently constructing for the ParserError to be returned
  // in the very first parser run (as long as we do not yet know whether we have to construct
  // a ParserError object) this value is -1
  private[this] var currentErrorRuleStackIx: Int = _

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  val __valueStack = new ValueStack

  def rule[I <: HList, O <: HList](r: Rule[I, O]): Rule[I, O] = macro ruleImpl[I, O]

  def run[L <: HList](rule: this.type ⇒ RuleN[L]): Either[Parser.Error, L] = {
    def runRule(errorRuleStackIx: Int = -1): Boolean = {
      cursor = -1
      __valueStack.clear()
      mismatchesAtErrorIndex = 0
      currentErrorRuleStackIx = errorRuleStackIx
      rule(this).matched
    }
    @tailrec def buildParseError(errorRuleIx: Int = 0,
                                 stacksBuilder: VectorBuilder[RuleStack] = new VectorBuilder): Error = {
      val ruleFrames: Seq[RuleFrame] =
        try {
          runRule(errorRuleIx)
          Nil // we managed to complete the run w/o exception, i.e. we have collected all frames
        } catch {
          case e: Parser.CollectingRuleStackException ⇒ e.ruleFrames
        }
      if (ruleFrames.isEmpty) Error(errorPosition(), stacksBuilder.result())
      else buildParseError(errorRuleIx + 1, stacksBuilder += RuleStack(ruleFrames))
    }
    errorIndex = 0
    if (runRule())
      Right(__valueStack.toHList[L]())
    else
      Left(buildParseError())
  }

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  def __nextChar(): Char = {
    val nextCursor = cursor + 1
    if (nextCursor < input.length) {
      cursor = nextCursor
      if (currentErrorRuleStackIx == -1 && nextCursor > errorIndex)
        errorIndex = nextCursor // if we are in the first "regular" parser run, we need to advance the errorIndex here
      input.charAt(nextCursor)
    } else EOI
  }

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  def __markCursorAndValueStack: Mark = new Mark((cursor.toLong << 32) + __valueStack.top)

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  def __resetCursorAndValueStack(mark: Mark): Unit = {
    cursor = (mark.value >>> 32).toInt
    __valueStack.top = (mark.value & 0x00000000FFFFFFFF).toInt
  }

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  def __markCursor: Int = cursor

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  def __resetCursor(mark: Int): Unit = cursor = mark

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  def __sliceInput(start: Int): String = input.sliceString(start + 1, cursor + 1)

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  def __onCharMismatch(): Boolean = {
    if (currentErrorRuleStackIx != -1 && cursor == errorIndex) {
      if (mismatchesAtErrorIndex < currentErrorRuleStackIx) mismatchesAtErrorIndex += 1
      else throw new Parser.CollectingRuleStackException
    }
    false
  }

  @tailrec
  private def errorPosition(ix: Int = math.min(errorIndex, input.length - 1), line: Int = 1, col: Int = -1): Position =
    if (ix < 0) Position(errorIndex, line, if (col == -1) errorIndex + 1 else col)
    else if (input.charAt(ix) != '\n') errorPosition(ix - 1, line, col)
    else errorPosition(ix - 1, line + 1, if (col == -1) errorIndex - ix else col)
}

object Parser {
  sealed trait Result[+L <: HList]
  case class Value[L <: HList](value: L) extends Result[L]
  case class Continuation[L <: HList](continuation: ParserInput ⇒ Result[L]) extends Result[L]
  case class Error(position: Position, errorRules: Seq[RuleStack]) extends Result[Nothing]
  case class Position(index: Int, line: Int, column: Int)
  case class RuleStack(frames: Seq[RuleFrame])

  class Mark private[Parser] (val value: Long) extends AnyVal

  type ParserContext = Context { type PrefixType = Parser }

  def ruleImpl[I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: ParserContext)(r: ctx.Expr[Rule[I, O]]): ctx.Expr[Rule[I, O]] = {
    val opTreeCtx = new OpTreeContext[ctx.type] { val c: ctx.type = ctx }
    val opTree = opTreeCtx.OpTree(r.tree)
    val ruleName = ctx.enclosingMethod.asInstanceOf[ctx.universe.DefDef].name.toString
    ctx.universe.reify {
      opTree.render(ruleName).splice.asInstanceOf[Rule[I, O]]
    }
  }

  /**
   * THIS IS NOT PUBLIC API. It will be hidden in future. Use it at your own risk.
   */
  class CollectingRuleStackException extends RuntimeException {
    private[this] val frameBuilder = new VectorBuilder[RuleFrame]

    def save(frame: RuleFrame): Nothing = {
      frameBuilder += frame
      throw this
    }

    def ruleFrames: Seq[RuleFrame] = frameBuilder.result()

    override def fillInStackTrace(): Throwable = this // skip stack trace creation as we don't need it
  }
}
