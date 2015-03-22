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
import scala.collection.immutable.VectorBuilder
import scala.util.control.{ NonFatal, NoStackTrace }
import shapeless.HList

sealed abstract class ParserState[Context] {

  /**
   * The input this parser instance is running against.
   */
  def input: ParserInput

  /**
   * The user context for the current parsing run.
   */
  def ctx: Context

  /**
   * The index of the next (yet unmatched) input character.
   * Might be equal to `input.length`!
   */
  def cursor: Int

  /**
   * The next (yet unmatched) input character, i.e. the one at the `cursor` index.
   * Identical to `if (cursor < input.length) input.charAt(cursor) else EOI` but more efficient.
   */
  def cursorChar: Char

  /**
   * Returns the last character that was matched, i.e. the one at index cursor - 1
   * Note: for performance optimization this method does *not* do a range check,
   * i.e. depending on the ParserInput implementation you might get an exception
   * when calling this method before any character was matched by the parser.
   */
  def lastChar: Char

  /**
   * Returns the character at the input index with the given delta to the cursor.
   * Note: for performance optimization this method does *not* do a range check,
   * i.e. depending on the ParserInput implementation you might get an exception
   * when calling this method before any character was matched by the parser.
   */
  def charAt(offset: Int): Char

  /**
   * Same as `charAt` but range-checked.
   * Returns the input character at the index with the given offset from the cursor.
   * If this index is out of range the method returns `EOI`.
   */
  def charAtRC(offset: Int): Char

  /**
   * Allows "raw" (i.e. untyped) access to the `ValueStack`.
   * In most cases you shouldn't need to access the value stack directly from your code.
   * Use only if you know what you are doing!
   */
  def valueStack: ValueStack
}

/**
 * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
 */
final class ParserStateImpl[C](val input: ParserInput, val ctx: C) extends ParserState[C] {
  import ParserStateImpl._

  // the char at the current input index
  private var _cursorChar: Char = _

  // the index of the current input char
  private var _cursor: Int = _

  // the value stack instance we operate on
  private var _valueStack: ValueStack = _

  // the current ErrorAnalysisPhase or null (in the initial run)
  private var phase: ErrorAnalysisPhase = _

  def cursor: Int = _cursor
  def cursorChar: Char = _cursorChar
  def lastChar: Char = charAt(-1)
  def valueStack: ValueStack = _valueStack
  def charAt(offset: Int): Char = input.charAt(_cursor + offset)
  def charAtRC(offset: Int): Char = {
    val ix = _cursor + offset
    if (0 <= ix && ix < input.length) input.charAt(ix) else EOI
  }

  def inErrorAnalysis = phase ne null

  def advance(): Boolean = {
    var c = _cursor
    val max = input.length
    if (c < max) {
      c += 1
      _cursor = c
      _cursorChar = if (c == max) EOI else input charAt c
    }
    true
  }

  def updateMaxCursor(): Boolean = {
    phase match {
      case x: EstablishingPrincipalErrorIndex ⇒ if (_cursor > x.maxCursor) x.maxCursor = _cursor
      case _                                  ⇒ // nothing to do
    }
    true
  }

  def saveState: Mark = new Mark((_cursor.toLong << 32) + (_cursorChar.toLong << 16) + valueStack.size)

  def restoreState(mark: Mark): Unit = {
    _cursor = (mark.value >>> 32).toInt
    _cursorChar = ((mark.value >>> 16) & 0x000000000000FFFF).toChar
    valueStack.size = (mark.value & 0x000000000000FFFF).toInt
  }

  def enterNotPredicate(): AnyRef = {
    val saved = phase
    phase = null
    saved
  }

  def exitNotPredicate(saved: AnyRef): Unit = phase = saved.asInstanceOf[ErrorAnalysisPhase]

  def enterAtomic(start: Int): Boolean =
    phase match {
      case null ⇒ false
      case x: EstablishingReportedErrorIndex if x.currentAtomicStart == Int.MinValue ⇒
        x.currentAtomicStart = start
        true
      case _ ⇒ false
    }

  def exitAtomic(saved: Boolean): Unit =
    if (saved) {
      phase match {
        case x: EstablishingReportedErrorIndex ⇒ x.currentAtomicStart = Int.MinValue
        case _                                 ⇒ throw new IllegalStateException
      }
    }

  def enterQuiet(): Int =
    phase match {
      case x: DetermineReportQuiet ⇒
        if (x.inQuiet) -1
        else {
          x.inQuiet = true
          0
        }
      case x: CollectingRuleTraces if !x.reportQuiet ⇒
        val saved = x.minErrorIndex
        x.minErrorIndex = Int.MaxValue // disables triggering of StartTracingException in __registerMismatch
        saved
      case _ ⇒ -1
    }

  def exitQuiet(saved: Int): Unit =
    if (saved >= 0) {
      phase match {
        case x: DetermineReportQuiet ⇒ x.inQuiet = false
        case x: CollectingRuleTraces ⇒ x.minErrorIndex = saved
        case _                       ⇒ throw new IllegalStateException
      }
    }

  def registerMismatch(): Boolean = {
    phase match {
      case null | _: EstablishingPrincipalErrorIndex ⇒ // nothing to do
      case x: CollectingRuleTraces ⇒
        if (_cursor >= x.minErrorIndex) {
          if (x.errorMismatches == x.traceNr) throw StartTracingException else x.errorMismatches += 1
        }
      case x: EstablishingReportedErrorIndex ⇒
        if (x.currentAtomicStart > x.maxAtomicErrorStart) x.maxAtomicErrorStart = x.currentAtomicStart
      case x: DetermineReportQuiet ⇒
        // stop this run early because we just learned that reporting quiet traces is unnecessary
        if (_cursor >= x.minErrorIndex & !x.inQuiet) throw UnquietMismatch
    }
    false
  }

  def bubbleUp(terminal: RuleTrace.Terminal): Nothing = bubbleUp(Nil, terminal)

  def bubbleUp(prefix: List[RuleTrace.NonTerminal], terminal: RuleTrace.Terminal): Nothing =
    throw new TracingBubbleException(RuleTrace(prefix, terminal))

  def push(value: Any): Boolean = {
    value match {
      case ()       ⇒
      case x: HList ⇒ valueStack.pushAll(x)
      case x        ⇒ valueStack.push(x)
    }
    true
  }

  @tailrec def matchString(string: String, ix: Int = 0): Boolean =
    if (ix < string.length)
      if (_cursorChar == string.charAt(ix)) {
        advance()
        matchString(string, ix + 1)
      } else false
    else true

  @tailrec def matchStringWrapped(string: String, ix: Int = 0): Boolean =
    if (ix < string.length)
      if (_cursorChar == string.charAt(ix)) {
        advance()
        updateMaxCursor()
        matchStringWrapped(string, ix + 1)
      } else {
        try registerMismatch()
        catch {
          case StartTracingException ⇒
            import RuleTrace._
            bubbleUp(NonTerminal(StringMatch(string), -ix) :: Nil, CharMatch(string charAt ix))
        }
      }
    else true

  @tailrec def matchIgnoreCaseString(string: String, ix: Int = 0): Boolean =
    if (ix < string.length)
      if (Character.toLowerCase(_cursorChar) == string.charAt(ix)) {
        advance()
        matchIgnoreCaseString(string, ix + 1)
      } else false
    else true

  @tailrec def matchIgnoreCaseStringWrapped(string: String, ix: Int = 0): Boolean =
    if (ix < string.length)
      if (Character.toLowerCase(_cursorChar) == string.charAt(ix)) {
        advance()
        updateMaxCursor()
        matchIgnoreCaseStringWrapped(string, ix + 1)
      } else {
        try registerMismatch()
        catch {
          case StartTracingException ⇒
            import RuleTrace._
            bubbleUp(NonTerminal(IgnoreCaseString(string), -ix) :: Nil, IgnoreCaseChar(string charAt ix))
        }
      }
    else true

  @tailrec def matchAnyOf(string: String, ix: Int = 0): Boolean =
    if (ix < string.length)
      if (string.charAt(ix) == _cursorChar) advance()
      else matchAnyOf(string, ix + 1)
    else false

  @tailrec def matchNoneOf(string: String, ix: Int = 0): Boolean =
    if (ix < string.length)
      _cursorChar != EOI && string.charAt(ix) != _cursorChar && matchNoneOf(string, ix + 1)
    else advance()

  def matchMap(m: Map[String, Any]): Boolean = {
    val keys = m.keysIterator
    while (keys.hasNext) {
      val mark = saveState
      val key = keys.next()
      if (matchString(key)) {
        push(m(key))
        return true
      } else restoreState(mark)
    }
    false
  }

  def matchMapWrapped(m: Map[String, Any]): Boolean = {
    val keys = m.keysIterator
    val start = _cursor
    try {
      while (keys.hasNext) {
        val mark = saveState
        val key = keys.next()
        if (matchStringWrapped(key)) {
          push(m(key))
          return true
        } else restoreState(mark)
      }
      false
    } catch {
      case e: TracingBubbleException ⇒ e.bubbleUp(RuleTrace.MapMatch(m), start)
    }
  }

  def hardFail(expected: String) = throw new Fail(expected)

  class TracingBubbleException(private var _trace: RuleTrace) extends RuntimeException with NoStackTrace {
    def trace = _trace
    def bubbleUp(key: RuleTrace.NonTerminalKey, start: Int): Nothing = throw prepend(key, start)
    def prepend(key: RuleTrace.NonTerminalKey, start: Int): this.type = {
      val offset = phase match {
        case x: CollectingRuleTraces ⇒ start - x.minErrorIndex
        case _                       ⇒ throw new IllegalStateException
      }
      _trace = _trace.copy(prefix = RuleTrace.NonTerminal(key, offset) :: _trace.prefix)
      this
    }
  }

  def run[L <: HList](rule: RuleImpl[C],
                      errorTraceCollectionLimit: Int,
                      initialValueStackSize: Int,
                      maxValueStackSize: Int)(implicit scheme: DeliveryScheme[L]): scheme.Result = {
    def runRule(): Boolean = {
      _cursor = -1
      advance()
      _valueStack.clear()
      try rule.run(this)
      catch {
        case CutError ⇒ false
      }
    }

    def phase0_initialRun() = {
      _valueStack = new ValueStack(initialValueStackSize, maxValueStackSize)
      runRule()
    }

    def phase1_establishPrincipalErrorIndex(): Int = {
      val phase1 = new EstablishingPrincipalErrorIndex()
      phase = phase1
      if (runRule()) sys.error("Parsing unexpectedly succeeded while trying to establish the principal error location")
      phase1.maxCursor
    }

    def phase2_establishReportedErrorIndex(principalErrorIndex: Int) = {
      val phase2 = new EstablishingReportedErrorIndex(principalErrorIndex)
      phase = phase2
      if (runRule()) sys.error("Parsing unexpectedly succeeded while trying to establish the reported error location")
      phase2
    }

    def phase3_determineReportQuiet(reportedErrorIndex: Int): Boolean = {
      phase = new DetermineReportQuiet(reportedErrorIndex)
      try {
        if (runRule()) sys.error("Parsing unexpectedly succeeded while trying to determine quiet reporting")
        true // if we got here we can only reach the reportedErrorIndex via quiet rules
      } catch {
        case UnquietMismatch ⇒ false // we mismatched beyond the reportedErrorIndex outside of a quiet rule
      }
    }

    @tailrec
    def phase4_collectRuleTraces(reportedErrorIndex: Int, principalErrorIndex: Int, reportQuiet: Boolean)(
      phase3: CollectingRuleTraces = new CollectingRuleTraces(reportedErrorIndex, reportQuiet),
      traces: VectorBuilder[RuleTrace] = new VectorBuilder): ParseError = {

      def done = {
        val principalErrorPos = Position(principalErrorIndex, input)
        val reportedErrorPos = if (reportedErrorIndex != principalErrorIndex) Position(reportedErrorIndex, input) else principalErrorPos
        ParseError(reportedErrorPos, principalErrorPos, traces.result())
      }
      if (phase3.traceNr < errorTraceCollectionLimit) {
        val trace: RuleTrace =
          try {
            phase = phase3
            runRule()
            null // we managed to complete the run w/o exception, i.e. we have collected all traces
          } catch {
            case e: TracingBubbleException ⇒ e.trace
          }
        if (trace eq null) done
        else phase4_collectRuleTraces(reportedErrorIndex, principalErrorIndex,
          reportQuiet)(new CollectingRuleTraces(reportedErrorIndex, reportQuiet, phase3.traceNr + 1), traces += trace)
      } else done
    }

    try {
      if (phase0_initialRun())
        scheme.success(valueStack.toHList[L]())
      else {
        val principalErrorIndex = phase1_establishPrincipalErrorIndex()
        val p2 = phase2_establishReportedErrorIndex(principalErrorIndex)
        val reportQuiet = phase3_determineReportQuiet(principalErrorIndex)
        val parseError = phase4_collectRuleTraces(p2.reportedErrorIndex, principalErrorIndex, reportQuiet)()
        scheme.parseError(parseError)
      }
    } catch {
      case e: Fail ⇒
        val pos = Position(cursor, input)
        scheme.parseError(ParseError(pos, pos, RuleTrace(Nil, RuleTrace.Fail(e.expected)) :: Nil))
      case NonFatal(e) ⇒
        e.printStackTrace()
        scheme.failure(e)
    } finally {
      phase = null
    }
  }
}

/**
 * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
 */
object ParserStateImpl {

  class Mark(val value: Long) extends AnyVal

  object StartTracingException extends RuntimeException with NoStackTrace
  object CutError extends RuntimeException with NoStackTrace
  object UnquietMismatch extends RuntimeException with NoStackTrace
  class Fail(val expected: String) extends RuntimeException with NoStackTrace

  // error analysis happens in 4 phases:
  // 0: initial run, no error analysis
  // 1: EstablishingPrincipalErrorIndex (1 run)
  // 2: EstablishingReportedErrorIndex (1 run)
  // 3: CollectingRuleTraces (n runs)
  sealed abstract class ErrorAnalysisPhase {
    def applyOffset(offset: Int): Unit
  }

  // establish the max cursor value reached in a run
  class EstablishingPrincipalErrorIndex(var maxCursor: Int = 0) extends ErrorAnalysisPhase {
    def applyOffset(offset: Int) = maxCursor -= offset
  }

  // establish the largest match-start index of all outermost atomic rules
  // that we are in when mismatching at the principal error index
  // or -1 if no atomic rule fails with a mismatch at the principal error index
  class EstablishingReportedErrorIndex(
      private var _principalErrorIndex: Int,
      var currentAtomicStart: Int = Int.MinValue,
      var maxAtomicErrorStart: Int = Int.MinValue) extends ErrorAnalysisPhase {
    def reportedErrorIndex = if (maxAtomicErrorStart >= 0) maxAtomicErrorStart else _principalErrorIndex
    def applyOffset(offset: Int) = {
      _principalErrorIndex -= offset
      if (currentAtomicStart != Int.MinValue) currentAtomicStart -= offset
      if (maxAtomicErrorStart != Int.MinValue) maxAtomicErrorStart -= offset
    }
  }

  // determine whether the reported error location can only be reached via quiet rules
  // in which case we need to report them even though they are marked as "quiet"
  class DetermineReportQuiet(
      private var _minErrorIndex: Int, // the smallest index at which a mismatch triggers a StartTracingException
      var inQuiet: Boolean = false // are we currently in a quiet rule?
      ) extends ErrorAnalysisPhase {
    def minErrorIndex = _minErrorIndex
    def applyOffset(offset: Int) = _minErrorIndex -= offset
  }

  // collect the traces of all mismatches happening at an index >= minErrorIndex (the reported error index)
  // by throwing a StartTracingException which gets turned into a TracingBubbleException by the terminal rule
  class CollectingRuleTraces(
      var minErrorIndex: Int, // the smallest index at which a mismatch triggers a StartTracingException
      val reportQuiet: Boolean, // do we need to trace mismatches from quiet rules?
      val traceNr: Int = 0, // the zero-based index number of the RuleTrace we are currently building
      var errorMismatches: Int = 0 // the number of times we have already seen a mismatch at >= minErrorIndex
      ) extends ErrorAnalysisPhase {
    def applyOffset(offset: Int) = minErrorIndex -= offset
  }
}
