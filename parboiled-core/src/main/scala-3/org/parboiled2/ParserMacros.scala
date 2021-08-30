/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2

import org.parboiled2.support.hlist.HList

private[parboiled2] trait ParserMacroMethods { parser: Parser =>

  /** Converts a compile-time only rule definition into the corresponding rule method implementation.
    */
  inline def rule[I <: HList, O <: HList](inline r: Rule[I, O]): Rule[I, O] = ${ ParserMacros.ruleImpl('parser, 'r) }

  /** Converts a compile-time only rule definition into the corresponding rule method implementation
    * with an explicitly given name.
    */
  inline def namedRule[I <: HList, O <: HList](name: String)(inline r: Rule[I, O]): Rule[I, O] = ${
    ParserMacros.nameRuleImpl('parser, 'name, 'r)
  }

}

private[parboiled2] trait RuleRunnable {

  /** THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
    */
  implicit class Runnable[L <: HList](rule: RuleN[L]) {
    def run()(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result = ???
  }
}

import scala.quoted._
class OpTreeContext(parser: Expr[Parser])(using Quotes) {
  sealed trait OpTree {
    def render(wrapped: Boolean): Expr[Boolean]
  }

  sealed abstract class NonTerminalOpTree extends OpTree {
    def bubbleUp(e: Expr[org.parboiled2.Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing]

    // renders a Boolean Tree
    def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{
        val start = $parser.cursor
        try ${ renderInner('start, wrapped) } catch {
          case e: org.parboiled2.Parser#TracingBubbleException => ${ bubbleUp('e, 'start) }
        }
      }
      else renderInner(Expr(-1) /* dummy, won't be used */, wrapped)

    // renders a Boolean Tree
    protected def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean]
  }

  sealed abstract class DefaultNonTerminalOpTree extends NonTerminalOpTree {
    def bubbleUp(e: Expr[org.parboiled2.Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing] = '{
      $e.bubbleUp($ruleTraceNonTerminalKey, $start)
    }
    def ruleTraceNonTerminalKey: Expr[RuleTrace.NonTerminalKey]
  }

  sealed abstract class TerminalOpTree extends OpTree {
    def bubbleUp: Expr[Nothing] = '{ $parser.__bubbleUp($ruleTraceTerminal) }
    def ruleTraceTerminal: Expr[RuleTrace.Terminal]

    final def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{
        try ${ renderInner(wrapped) } catch { case org.parboiled2.Parser.StartTracingException => $bubbleUp }
      }
      else renderInner(wrapped)

    protected def renderInner(wrapped: Boolean): Expr[Boolean]
  }
  sealed abstract class PotentiallyNamedTerminalOpTree extends TerminalOpTree {
    // TODO
  }

  def Sequence(lhs: OpTree, rhs: OpTree): Sequence =
    lhs -> rhs match {
      case (Sequence(lops), Sequence(rops)) => Sequence(lops ++ rops)
      case (Sequence(lops), _)              => Sequence(lops :+ rhs)
      case (_, Sequence(ops))               => Sequence(lhs +: ops)
      case _                                => Sequence(Seq(lhs, rhs))
    }
  case class Sequence(ops: Seq[OpTree]) extends OpTree {
    override def render(wrapped: Boolean): Expr[Boolean] =
      ops
        .map(_.render(wrapped))
        .reduceLeft((l, r) => '{ val ll = $l; if (ll) $r else false })
  }

  case class Capture(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = '{ RuleTrace.Capture }
    def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean] =
      '{
        val start1  = ${ if (wrapped) start else '{ $parser.cursor } }
        val matched = ${ op.render(wrapped) }
        if (matched) {
          $parser.valueStack.push($parser.input.sliceString(start1, $parser.cursor))
          true
        } else false
      }
  }

  case class CharMatch(charTree: Expr[Char]) extends TerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.CharMatch($charTree) }
    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{
        $parser.cursorChar == $charTree && $parser.__advance()
      }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case class StringMatch(stringTree: Expr[String]) extends OpTree {
    final private val autoExpandMaxStringLength = 8

    override def render(wrapped: Boolean): Expr[Boolean] =
      // TODO: add optimization for literal constant
      // def unrollUnwrapped(s: String, ix: Int = 0): Tree =
      //   if (ix < s.length) q"""
      //     if (cursorChar == ${s charAt ix}) {
      //       __advance()
      //       ${unrollUnwrapped(s, ix + 1)}:Boolean
      //     } else false"""
      //   else q"true"
      // def unrollWrapped(s: String, ix: Int = 0): Tree =
      //   if (ix < s.length) {
      //     val ch = s charAt ix
      //     q"""if (cursorChar == $ch) {
      //       __advance()
      //       __updateMaxCursor()
      //       ${unrollWrapped(s, ix + 1)}
      //     } else {
      //       try __registerMismatch()
      //       catch {
      //         case org.parboiled2.Parser.StartTracingException =>
      //           import org.parboiled2.RuleTrace._
      //           __bubbleUp(NonTerminal(StringMatch($stringTree), -$ix) :: Nil, CharMatch($ch))
      //       }
      //     }"""
      //   } else q"true"

      // stringTree match {
      //   case Literal(Constant(s: String)) if s.length <= autoExpandMaxStringLength =>
      //     if (s.isEmpty) q"true" else if (wrapped) unrollWrapped(s) else unrollUnwrapped(s)
      //   case _ =>
      //     if (wrapped) q"__matchStringWrapped($stringTree)"
      //     else q"__matchString($stringTree)"
      // }
      if (wrapped) '{ $parser.__matchStringWrapped($stringTree) }
      else '{ $parser.__matchString($stringTree) }
  }

  case class MapMatch(mapTree: Expr[Map[String, Any]], ignoreCaseTree: Expr[Boolean]) extends OpTree {

    override def render(wrapped: Boolean): Expr[Boolean] =
      if (wrapped) '{ $parser.__matchMapWrapped($mapTree, $ignoreCaseTree) }
      else '{ $parser.__matchMap($mapTree, $ignoreCaseTree) }
  }

  case class IgnoreCaseChar(charTree: Expr[Char]) extends TerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.IgnoreCaseChar($charTree) }

    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{
        _root_.java.lang.Character.toLowerCase($parser.cursorChar) == $charTree && $parser.__advance()
      }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case class IgnoreCaseString(stringTree: Expr[String]) extends OpTree {
    final private val autoExpandMaxStringLength = 8

    override def render(wrapped: Boolean): Expr[Boolean] =
      // TODO: optimize litera constant
      // def unrollUnwrapped(s: String, ix: Int = 0): Tree =
      //   if (ix < s.length) q"""
      //     if (_root_.java.lang.Character.toLowerCase(cursorChar) == ${s charAt ix}) {
      //       __advance()
      //       ${unrollUnwrapped(s, ix + 1)}
      //     } else false"""
      //   else q"true"
      // def unrollWrapped(s: String, ix: Int = 0): Tree =
      //   if (ix < s.length) {
      //     val ch = s charAt ix
      //     q"""if (_root_.java.lang.Character.toLowerCase(cursorChar) == $ch) {
      //       __advance()
      //       __updateMaxCursor()
      //       ${unrollWrapped(s, ix + 1)}
      //     } else {
      //       try __registerMismatch()
      //       catch {
      //         case org.parboiled2.Parser.StartTracingException =>
      //           import org.parboiled2.RuleTrace._
      //           __bubbleUp(NonTerminal(IgnoreCaseString($stringTree), -$ix) :: Nil, IgnoreCaseChar($ch))
      //       }
      //     }"""
      //   } else q"true"

      // stringTree match {
      //   case Literal(Constant(s: String)) if s.length <= autoExpandMaxStringLength =>
      //     if (s.isEmpty) q"true" else if (wrapped) unrollWrapped(s) else unrollUnwrapped(s)
      //   case _ =>
      //     if (wrapped) q"__matchIgnoreCaseStringWrapped($stringTree)"
      //     else q"__matchIgnoreCaseString($stringTree)"
      // }
      if (wrapped) '{ $parser.__matchIgnoreCaseStringWrapped($stringTree) }
      else '{ $parser.__matchIgnoreCaseString($stringTree) }
  }

  case class CharPredicateMatch(predicateTree: Expr[CharPredicate]) extends PotentiallyNamedTerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.CharPredicateMatch($predicateTree) }

    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{ $predicateTree($parser.cursorChar) && $parser.__advance() }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case class AnyOf(stringTree: Expr[String]) extends TerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.AnyOf($stringTree) }

    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{ $parser.__matchAnyOf($stringTree) }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case class NoneOf(stringTree: Expr[String]) extends TerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.NoneOf($stringTree) }

    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{ $parser.__matchNoneOf($stringTree) }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case object ANY extends TerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.ANY }

    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{ $parser.cursorChar != EOI && $parser.__advance() }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case class Unknown(syntax: String, tree: String, outerSyntax: String) extends TerminalOpTree {
    override def ruleTraceTerminal: Expr[RuleTrace.Terminal] =
      '{ RuleTrace.Fail(s"unknown rule: $$infoExpr") }

    override protected def renderInner(wrapped: Boolean): Expr[Boolean] =
      '{
        throw new RuntimeException(
          s"unknown rule: [${${ Expr(syntax) }}] '${${ Expr(tree) }} in [${${ Expr(outerSyntax) }}]"
        )
      }
  }

  def CharRange(lowerTree: Expr[String], upperTree: Expr[String]): CharacterRange =
    (lowerTree.value, upperTree.value) match {
      case (Some(lower), Some(upper)) =>
        if (lower.length != 1) reportError("lower bound must be a single char string", lowerTree)
        if (upper.length != 1) reportError("upper bound must be a single char string", upperTree)
        val lowerBoundChar = lower.charAt(0)
        val upperBoundChar = upper.charAt(0)
        if (lowerBoundChar > upperBoundChar) reportError("lower bound must not be > upper bound", lowerTree)
        CharacterRange(Expr(lowerBoundChar), Expr(upperBoundChar))
      case _ => reportError("Character ranges must be specified with string literals", lowerTree)
    }

  case class CharacterRange(lowerBound: Expr[Char], upperBound: Expr[Char]) extends TerminalOpTree {
    def ruleTraceTerminal = '{ org.parboiled2.RuleTrace.CharRange($lowerBound, $upperBound) }

    override def renderInner(wrapped: Boolean): Expr[Boolean] = {
      val unwrappedTree = '{
        val char = $parser.cursorChar
        $lowerBound <= char && char <= $upperBound && $parser.__advance()
      }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  def deconstruct(outerRule: Expr[Rule[_, _]]): OpTree = {
    import quotes.reflect.*
    def rec(rule: Term): OpTree = rule.asExprOf[Rule[_, _]] match {
      case '{ ($p: Parser).ch($c) }                              => CharMatch(c)
      case '{ ($p: Parser).str($s) }                             => StringMatch(s)
      case '{ ($p: Parser).valueMap($m: Map[String, Any]) }      => MapMatch(m, '{ false })
      case '{ ($p: Parser).valueMap($m: Map[String, Any], $ic) } => MapMatch(m, ic)
      case '{ ($p: Parser).ignoreCase($c: Char) }                => IgnoreCaseChar(c)
      case '{ ($p: Parser).ignoreCase($s: String) }              => IgnoreCaseString(s)
      case '{ ($p: Parser).predicate($pr) }                      => CharPredicateMatch(pr)
      case '{ ($p: Parser).anyOf($s) }                           => AnyOf(s)
      case '{ ($p: Parser).noneOf($s) }                          => NoneOf(s)
      case '{ ($p: Parser).ANY }                                 => ANY
      case '{ ($p: Parser).str2CharRangeSupport($l).-($r) }      => CharRange(l, r)
      case x                                                     =>
        // These patterns cannot be parsed as quoted patterns because of the complicated type applies
        x.asTerm.underlyingArgument match {
          case Apply(Apply(TypeApply(Select(lhs, "~"), _), List(rhs)), _) =>
            Sequence(Seq(rec(lhs), rec(rhs)))
          case Apply(Apply(TypeApply(Select(_, "capture"), _), List(arg)), _) =>
            Capture(rec(arg))
          case _ =>
            Unknown(x.toString, rule.show, outerRule.toString)
        }
    }

    rec(outerRule.asTerm)
  }

  private def reportError(error: String, expr: Expr[Any]): Nothing = {
    quotes.reflect.report.error(error, expr)
    throw new scala.quoted.runtime.StopMacroExpansion
  }
}

object ParserMacros {
  import scala.quoted._
  import scala.compiletime._

  def ruleImpl[I <: HList: Type, O <: HList: Type](parser: Expr[Parser], r: Expr[Rule[I, O]])(using
      Quotes
  ): Expr[Rule[I, O]] = {
    import quotes.reflect.*
    nameRuleImpl(parser, Expr(Symbol.spliceOwner.owner.name), r)
  }

  def nameRuleImpl[I <: HList: Type, O <: HList: Type](parser: Expr[Parser], name: Expr[String], r: Expr[Rule[I, O]])(
      using Quotes
  ): Expr[Rule[I, O]] = {
    import quotes.reflect.*

    val ctx    = new OpTreeContext(parser)
    val opTree = ctx.deconstruct(r)

    '{
      def wrapped: Boolean = ${ opTree.render(wrapped = true) }
      val matched =
        if ($parser.__inErrorAnalysis) wrapped
        else ${ opTree.render(wrapped = false) }
      if (matched) org.parboiled2.Rule.asInstanceOf[Rule[I, O]] else null
    }
  }
}
