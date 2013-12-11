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
import scala.util.control.NoStackTrace

abstract class Parser extends RuleDSL {
  import Parser._

  def input: ParserInput

  def rule[I <: HList, O <: HList](r: Rule[I, O]): Rule[I, O] = macro ruleImpl[I, O]

  /**
   * Pretty prints the given `ParseError` instance in the context of the `ParserInput` of this parser.
   */
  def formatError(error: ParseError, showTraces: Boolean = false): String = {
    val ParseError(pos @ Position(index, line, col), traces) = error
    val errorChar = if (index < input.length) input charAt index else EOI
    val expected: Vector[String] =
      traces.map { trace ⇒
        val exp = trace.frames.last.format
        if (exp.isEmpty) "?" else exp
      }(collection.breakOut)
    val caret = " " * (col - 1) + '^'
    val errorMsg = formatError(errorChar, pos, expected, input getLine line, caret)
    if (showTraces) errorMsg + "\n\n" + formatErrorTraces(traces) else errorMsg
  }

  /**
   * Pretty prints the given `ParseError`.
   */
  def formatError(errorChar: Char, pos: Position, expected: Seq[String], line: String, caret: String): String = {
    val problem = if (errorChar == EOI) "Unexpected end of input" else s"Invalid input '$errorChar'"
    val exp = if (expected.size == 1) expected.head else expected.init.mkString(", ") + " or " + expected.last
    s"$problem, expected $exp (line ${pos.line}, column ${pos.column}):\n$line\n$caret"
  }

  /**
   * Pretty prints the given error rule traces.
   */
  def formatErrorTraces(traces: Seq[RuleTrace]): String =
    traces.map(_.format).mkString("Mismatched rules at error location:\n  ", "\n  ", "\n")

  ////////////////////// INTERNAL /////////////////////////

  // the char at the current input index
  private[this] var currentChar: Char = _

  // the index of the current input char
  private[this] var currentIndex: Int = _

  // the highest input index we have seen in the current run
  private[this] var maxIndex: Int = _

  // the number of times we have already seen a character mismatch at the error index
  private[this] var mismatchesAtErrorIndex: Int = _

  // the index of the RuleStack we are currently constructing
  // for the ParserError to be returned in the very first parser run,
  // as long as we do not yet know whether we have to construct a ParserError object this value is -1
  private[this] var currentErrorRuleStackIx: Int = _

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  val __valueStack = new ValueStack

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __run[L <: HList](rule: ⇒ RuleN[L]): Result[L] = {
    def runRule(errorRuleStackIx: Int = -1): Boolean = {
      currentIndex = -1
      __advance()
      __valueStack.clear()
      mismatchesAtErrorIndex = 0
      currentErrorRuleStackIx = errorRuleStackIx
      rule.matched
    }

    @tailrec
    def errorPosition(ix: Int = math.min(maxIndex, input.length - 1), line: Int = 1, col: Int = -1): Position =
      if (ix < 0) Position(maxIndex, line, if (col == -1) maxIndex + 1 else col)
      else if (input.charAt(ix) != '\n') errorPosition(ix - 1, line, col)
      else errorPosition(ix - 1, line + 1, if (col == -1) maxIndex - ix else col)

    @tailrec
    def buildParseError(errorRuleIx: Int = 0, traces: VectorBuilder[RuleTrace] = new VectorBuilder): ParseError = {
      val ruleFrames: List[RuleFrame] =
        try {
          runRule(errorRuleIx)
          Nil // we managed to complete the run w/o exception, i.e. we have collected all frames
        } catch {
          case e: Parser.CollectingRuleStackException ⇒ e.ruleFrames
        }
      if (ruleFrames.isEmpty) ParseError(errorPosition(), traces.result())
      else buildParseError(errorRuleIx + 1, traces += RuleTrace(ruleFrames.toVector))
    }

    maxIndex = -1
    if (runRule())
      Right(__valueStack.toHList[L]())
    else
      Left(buildParseError())
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __currentChar: Char = currentChar

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __advance(): Unit = {
    var cursor = currentIndex
    val max = input.length
    if (cursor < max) {
      cursor += 1
      currentIndex = cursor
      currentChar =
        if (cursor == max) EOI
        else input charAt cursor
      if (currentErrorRuleStackIx == -1 && cursor > maxIndex)
        maxIndex = cursor // if we are in the first "regular" parser run, we need to advance the errorIndex here
    }
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __saveState: Mark = new Mark((currentIndex.toLong << 32) + (currentChar.toLong << 16) + __valueStack.top)

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __restoreState(mark: Mark): Unit = {
    currentIndex = (mark.value >>> 32).toInt
    currentChar = ((mark.value >>> 16) & 0x000000000000FFFF).toChar
    __valueStack.top = (mark.value & 0x000000000000FFFF).toInt
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __markCursor: Int = currentIndex

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __resetCursor(mark: Int): Unit = currentIndex = mark

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __sliceInput(start: Int): String = input.sliceString(start, currentIndex)

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __registerCharMismatch(): Unit =
    if (currentErrorRuleStackIx != -1 && currentIndex == maxIndex) {
      if (mismatchesAtErrorIndex < currentErrorRuleStackIx) mismatchesAtErrorIndex += 1
      else throw new Parser.CollectingRuleStackException
    }
}

object Parser {
  class Mark private[Parser] (val value: Long) extends AnyVal

  // TODO: please everyone all the time
  type Result[L <: HList] = Either[ParseError, L]

  type RunnableRuleContext[L <: HList] = Context { type PrefixType = Rule.Runnable[L] }

  def runImpl[L <: HList: ctx.WeakTypeTag](ctx: RunnableRuleContext[L])(): ctx.Expr[Result[L]] = {
    import ctx.universe._
    ctx.prefix.tree match {
      case q"parboiled2.this.Rule.Runnable[$l]($parser.$rule)" ⇒ ctx.Expr[Result[L]](q"$parser.__run[$l]($parser.$rule)")
      case x ⇒ ctx.abort(x.pos, "Illegal `run` call: " + show(x))
    }
  }

  type ParserContext = Context { type PrefixType = Parser }

  def ruleImpl[I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: ParserContext)(r: ctx.Expr[Rule[I, O]]): ctx.Expr[Rule[I, O]] = {
    val opTreeCtx = new OpTreeContext[ctx.type] { val c: ctx.type = ctx }
    val opTree = opTreeCtx.OpTree(r.tree)
    import ctx.universe._
    val ruleName =
      ctx.enclosingMethod match {
        case q"def $name[..$tparams](...$vparamss): $tpt = $body" ⇒ name.toString
        case _ ⇒ ctx.abort(r.tree.pos, "`rule` can only be used from within a method")
      }
    reify {
      opTree.render(ruleName).splice.asInstanceOf[Rule[I, O]]
    }
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  class CollectingRuleStackException extends RuntimeException with NoStackTrace {
    private[this] var frames = List.empty[RuleFrame]
    def save(frame: RuleFrame): Nothing = {
      frames ::= frame
      throw this
    }
    def ruleFrames: List[RuleFrame] = frames
  }
}
