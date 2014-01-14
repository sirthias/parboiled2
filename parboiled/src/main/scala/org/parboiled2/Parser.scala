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
import scala.util.{ Failure, Success, Try }
import scala.util.control.{ NonFatal, NoStackTrace }
import shapeless._
import org.parboiled2.support._

abstract class Parser(initialValueStackSize: Int = 32,
                      maxValueStackSize: Int = 1024) extends RuleDSL {
  import Parser._

  require(maxValueStackSize <= 65536, "`maxValueStackSize` > 2^16 is not supported") // due to current snapshot design

  /**
   * The input this parser instance is running against.
   */
  def input: ParserInput

  /**
   * Converts a compile-time only rule definition into the corresponding rule method implementation.
   */
  def rule[I <: HList, O <: HList](r: Rule[I, O]): Rule[I, O] = macro ruleImpl[I, O]

  /**
   * The index of the next (yet unmatched) input character.
   * Might be equal to `input.length`!
   */
  def cursor: Int = _cursor

  /**
   * The next (yet unmatched) input character, i.e. the one at the `cursor` index.
   * Identical to `if (cursor < input.length) input.charAt(cursor) else EOI` but more efficient.
   */
  def cursorChar: Char = _cursorChar

  /**
   * Returns the input character at the index with the given offset from the cursor.
   * If this index is out of range the method returns `EOI`.
   */
  def cursorChar(offset: Int): Char = {
    val ix = cursor + offset
    if (0 <= ix && ix < input.length) input.charAt(ix) else EOI
  }

  /**
   * Returns the last character that was matched, i.e. the one at index cursor - 1
   * Note: for performance optimization this method does *not* do a range check,
   * i.e. depending on the ParserInput implementation you might get an exception
   * when calling this method before any character was matched by the parser.
   */
  def lastChar: Char = input.charAt(cursor - 1)

  /**
   * Allows "raw" (i.e. untyped) access to the `ValueStack`.
   * In most cases you shouldn't need to access the value stack directly from your code.
   * Use only if you know what you are doing!
   */
  val valueStack = new ValueStack(initialValueStackSize, maxValueStackSize)

  /**
   * Pretty prints the given `ParseError` instance in the context of the `ParserInput` of this parser.
   */
  def formatError(error: ParseError, showTraces: Boolean = false): String = {
    val ParseError(pos @ Position(index, line, col), traces) = error
    val errorChar = if (index < input.length) input charAt index else EOI
    val expected: Vector[String] =
      traces.map { trace ⇒
        if (trace.frames.nonEmpty) {
          val exp = RuleFrame.format(trace.frames.last)
          val nonEmptyExp = if (exp.isEmpty) "?" else exp
          if (trace.isNegated) "!" + nonEmptyExp else nonEmptyExp
        } else "???"

      }(collection.breakOut)
    val caret = " " * (col - 1) + '^'
    val errorMsg = formatError(errorChar, pos, expected, input getLine line, caret)
    if (showTraces) errorMsg + "\n\n" + formatErrorTraces(traces) else errorMsg
  }

  /**
   * Pretty prints the given `ParseError`.
   */
  def formatError(errorChar: Char, pos: Position, expected: Seq[String], line: String, caret: String): String = {
    val problem = if (errorChar == EOI) "Unexpected end of input" else s"Invalid input '${CharUtils.escape(errorChar)}'"
    val exp =
      expected.size match {
        case 0 ⇒ "??"
        case 1 ⇒ expected.head
        case _ ⇒ expected.init.mkString(", ") + " or " + expected.last
      }
    s"$problem, expected $exp (line ${pos.line}, column ${pos.column}):\n$line\n$caret"
  }

  /**
   * Pretty prints the given error rule traces.
   */
  def formatErrorTraces(traces: Seq[RuleTrace]): String =
    traces.map(_.format).mkString(traces.size + " rule" + (if (traces.size > 1) "s" else "") +
      " mismatched at error location:\n  ", "\n  ", "\n")

  ////////////////////// INTERNAL /////////////////////////

  // the char at the current input index
  private[this] var _cursorChar: Char = _

  // the index of the current input char
  private[this] var _cursor: Int = _

  // the highest input index we have seen in the current run
  private[this] var maxCursor: Int = _

  // the number of times we have already seen a character mismatch at the error index
  private[this] var mismatchesAtErrorCursor: Int = _

  // the index of the RuleStack we are currently constructing
  // for the ParseError to be (potentially) returned in the current parser run,
  // as long as we do not yet know whether we have to construct a ParseError object this value is -1
  private[this] var currentErrorRuleStackIx: Int = _

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __collectingErrors = currentErrorRuleStackIx >= 0

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __run[L <: HList](rule: ⇒ RuleN[L])(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result = {
    def runRule(errorRuleStackIx: Int = -1): Boolean = {
      _cursor = -1
      __advance()
      valueStack.clear()
      mismatchesAtErrorCursor = 0
      currentErrorRuleStackIx = errorRuleStackIx
      rule.matched
    }

    @tailrec
    def errorPosition(ix: Int = math.min(maxCursor, input.length - 1), line: Int = 1, col: Int = -1): Position =
      if (ix < 0) Position(maxCursor, line, if (col == -1) maxCursor + 1 else col)
      else if (input.charAt(ix) != '\n') errorPosition(ix - 1, line, col)
      else errorPosition(ix - 1, line + 1, if (col == -1) maxCursor - ix else col)

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

    try {
      maxCursor = -1
      if (runRule())
        scheme.success(valueStack.toHList[L]())
      else
        scheme.parseError(buildParseError())
    } catch {
      case NonFatal(e) ⇒ scheme.failure(e)
    }
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __advance(): Boolean = {
    var c = _cursor
    val max = input.length
    if (c < max) {
      c += 1
      _cursor = c
      _cursorChar =
        if (c == max) EOI
        else input charAt c
      if (currentErrorRuleStackIx == -1 && c > maxCursor)
        maxCursor = c // if we are in the first "regular" parser run, we need to keep track of maxCursor here
    }
    true
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __saveState: Mark = new Mark((_cursor.toLong << 32) + (_cursorChar.toLong << 16) + valueStack.size)

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __restoreState(mark: Mark): Unit = {
    _cursor = (mark.value >>> 32).toInt
    _cursorChar = ((mark.value >>> 16) & 0x000000000000FFFF).toChar
    valueStack.size = (mark.value & 0x000000000000FFFF).toInt
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __enterNotPredicate: Int = {
    val saved = currentErrorRuleStackIx
    currentErrorRuleStackIx = -2 // disables maxCursor update as well as error rulestack collection
    saved
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __exitNotPredicate(saved: Int): Unit = currentErrorRuleStackIx = saved

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __registerMismatch(): Boolean = {
    if (currentErrorRuleStackIx >= 0 && _cursor == maxCursor) {
      if (mismatchesAtErrorCursor < currentErrorRuleStackIx) mismatchesAtErrorCursor += 1
      else throw new Parser.CollectingRuleStackException
    }
    false
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  def __push(value: Any): Boolean = {
    value match {
      case ()       ⇒
      case x: HList ⇒ valueStack.pushAll(x)
      case x        ⇒ valueStack.push(x)
    }
    true
  }
}

object Parser {

  trait DeliveryScheme[L <: HList] {
    type Result
    def success(result: L): Result
    def parseError(error: ParseError): Result
    def failure(error: Throwable): Result
  }

  object DeliveryScheme extends AlternativeDeliverySchemes {
    implicit def Try[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) =
      new DeliveryScheme[L] {
        type Result = Try[Out]
        def success(result: L) = Success(unpack(result))
        def parseError(error: ParseError) = Failure(error)
        def failure(error: Throwable) = Failure(error)
      }
  }
  sealed abstract class AlternativeDeliverySchemes {
    implicit def Either[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) =
      new DeliveryScheme[L] {
        type Result = Either[ParseError, Out]
        def success(result: L) = Right(unpack(result))
        def parseError(error: ParseError) = Left(error)
        def failure(error: Throwable) = throw error
      }
    implicit def Throw[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) =
      new DeliveryScheme[L] {
        type Result = Out
        def success(result: L) = unpack(result)
        def parseError(error: ParseError) = throw error
        def failure(error: Throwable) = throw error
      }
  }

  ////////////////////////////// INTERNAL //////////////////////////////

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  class Mark private[Parser] (val value: Long) extends AnyVal

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  type RunnableRuleContext[L <: HList] = Context { type PrefixType = Rule.Runnable[L] }

  def runImpl[L <: HList: c.WeakTypeTag](c: RunnableRuleContext[L])()(scheme: c.Expr[DeliveryScheme[L]]): c.Expr[scheme.value.Result] = {
    import c.universe._
    c.prefix.tree match {
      case q"parboiled2.this.Rule.Runnable[$l]($parser.$rule)" ⇒
        c.Expr[scheme.value.Result](q"val p = $parser; p.__run[$l](p.$rule)($scheme)")
      case x ⇒ c.abort(x.pos, "Illegal `Runnable.apply` call: " + x)
    }
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  type ParserContext = Context { type PrefixType = Parser }

  def ruleImpl[I <: HList: ctx.WeakTypeTag, O <: HList: ctx.WeakTypeTag](ctx: ParserContext)(r: ctx.Expr[Rule[I, O]]): ctx.Expr[Rule[I, O]] = {
    val opTreeCtx = new OpTreeContext[ctx.type] { val c: ctx.type = ctx }
    val opTree = opTreeCtx.OpTree(r.tree)
    import ctx.universe._
    val ruleName =
      ctx.enclosingMethod match {
        case DefDef(_, name, _, _, _, _) ⇒ name.decoded
        case _                           ⇒ ctx.abort(r.tree.pos, "`rule` can only be used from within a method")
      }
    reify {
      ctx.Expr[RuleX](opTree.renderRule(ruleName)).splice.asInstanceOf[Rule[I, O]]
    }
  }

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  class CollectingRuleStackException extends RuntimeException with NoStackTrace {
    private[this] var frames = List.empty[RuleFrame]
    def save(newFrames: RuleFrame*): Nothing = {
      frames = newFrames.foldRight(frames)(_ :: _)
      throw this
    }
    def ruleFrames: List[RuleFrame] = frames
  }
}
