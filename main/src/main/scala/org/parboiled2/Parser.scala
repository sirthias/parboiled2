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

abstract class Parser {
  type Result = Either[ParseError, Unit]

  def input: ParserInput

  // the parser supports dropping an input marker and later resetting the parser to this marker
  // `Mark` defined the type of this (opaque) marker, for now it is simply the cursor value
  type Mark = Int

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

  def rule(r: Rule): Rule = macro Parser.ruleImpl

  def run(rule: this.type ⇒ Rule): Result = {
    def runRule(errorRuleStackIx: Int = -1): Boolean = {
      cursor = -1
      mismatchesAtErrorIndex = 0
      currentErrorRuleStackIx = errorRuleStackIx
      rule(this).matched
    }
    @tailrec def buildParseError(errorRuleIx: Int = 0,
                                 stacksBuilder: VectorBuilder[RuleStack] = new VectorBuilder): ParseError = {
      val ruleFrames: Seq[RuleFrame] =
        try {
          runRule(errorRuleIx)
          Nil // we managed to complete the run w/o exception, i.e. we have collected all frames
        } catch {
          case e: Parser.CollectingRuleStackException ⇒ e.ruleFrames
        }
      if (ruleFrames.isEmpty) ParseError(errorPosition(), stacksBuilder.result())
      else buildParseError(errorRuleIx + 1, stacksBuilder += RuleStack(ruleFrames))
    }
    errorIndex = 0
    if (runRule()) Right(Unit)
    else Left(buildParseError())
  }

  implicit def ch(c: Char) = Rule()
  implicit def str(s: String) = Rule()
  def zeroOrMore(r: Rule) = Rule()
  def oneOrMore(r: Rule) = Rule()
  def optional(r: Rule) = Rule()
  def &(r: Rule): Rule = Rule()

  def nextChar(): Char = {
    val nextCursor = cursor + 1
    if (nextCursor < input.length) {
      cursor = nextCursor
      if (currentErrorRuleStackIx == -1 && nextCursor > errorIndex)
        errorIndex = nextCursor // if we are in the first "regular" parser run, we need to advance the errorIndex here
      input.charAt(nextCursor)
    } else EOI
  }

  def mark: Mark = cursor
  def reset(mark: Mark): Unit = cursor = mark

  def onCharMismatch(): Unit =
    if (currentErrorRuleStackIx != -1 && cursor == errorIndex) {
      if (mismatchesAtErrorIndex < currentErrorRuleStackIx) mismatchesAtErrorIndex += 1
      else throw new Parser.CollectingRuleStackException
    }

  @tailrec
  private def errorPosition(ix: Int = math.min(errorIndex, input.length - 1), line: Int = 1, col: Int = -1): Position =
    if (ix < 0) Position(errorIndex, line, if (col == -1) errorIndex + 1 else col)
    else if (input.charAt(ix) != '\n') errorPosition(ix - 1, line, col)
    else errorPosition(ix - 1, line + 1, if (col == -1) errorIndex - ix else col)

  def EOI = Parser.EOI
}

object Parser {
  val EOI: Char = '\uFFFF'

  type ParserContext = Context { type PrefixType = Parser }

  def ruleImpl(ctx: ParserContext)(r: ctx.Expr[Rule]): ctx.Expr[Rule] = {
    val opTreeCtx = new OpTreeContext[ctx.type] { val c: ctx.type = ctx }
    val opTree = opTreeCtx.OpTree(r.tree)
    val ctx.universe.DefDef(_, ruleName, _, _, _, _) = ctx.enclosingMethod
    opTree.render(ruleName.toString)
  }

  private[parboiled2] class CollectingRuleStackException extends RuntimeException {
    private[this] val frameBuilder = new VectorBuilder[RuleFrame]

    def save(frame: RuleFrame): Nothing = {
      frameBuilder += frame
      throw this
    }

    def ruleFrames: Seq[RuleFrame] = frameBuilder.result()

    override def fillInStackTrace(): Throwable = this // skip stack trace creation as we don't need it
  }
}
