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

private[parboiled2] trait ParserMacroMethods {

  /** Converts a compile-time only rule definition into the corresponding rule method implementation.
    */
  inline def rule[I <: HList, O <: HList](inline r: Rule[I, O]): Rule[I, O] = ${ ParserMacros.ruleImpl('r) }

  /** Converts a compile-time only rule definition into the corresponding rule method implementation
    * with an explicitly given name.
    */
  inline def namedRule[I <: HList, O <: HList](name: String)(inline r: Rule[I, O]): Rule[I, O] = ${
    ParserMacros.nameRuleImpl('name)('r)
  }

}

private[parboiled2] trait RuleRunnable {

  /** THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
    */
  implicit class Runnable[L <: HList](rule: RuleN[L]) {
    def run()(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result = ???
  }
}

object ParserMacros {
  import scala.quoted._
  import scala.compiletime._

  def ruleImpl[I <: HList: Type, O <: HList: Type](r: Expr[Rule[I, O]])(using Quotes): Expr[Rule[I, O]] =
    nameRuleImpl(Expr("todo"))(r)

  def nameRuleImpl[I <: HList: Type, O <: HList: Type](
      name: Expr[String]
  )(r: Expr[Rule[I, O]])(using Quotes): Expr[Rule[I, O]] = {
    import quotes.reflect.*

    def opTreeF(rule: Expr[Rule[I, O]]): OpTree = rule match {
      case '{
            (${ lhs }: Rule[I, O])
              .~((${ rhs }: Rule[I, O]))($c, $d)
          } =>
        Sequence(Seq(opTreeF(lhs), opTreeF(rhs)))
      case '{ ($p: Parser).ch($c) } =>
        CharMatch(p, c)
      case '{ ($p: Parser).str($s) } =>
        StringMatch(p, s)
      case '{ ($p: Parser).predicate($pr) } =>
        CharPredicateMatch(p, pr)
      case _ => reportError(s"Invalid rule definition: '${r.show}';", r)
    }

    val opTree: OpTree = opTreeF(r)

    val parser = opTree.parser
    '{
      def wrapped: Boolean = ${ opTree.render(wrapped = true) }
      val matched =
        if ($parser.__inErrorAnalysis) wrapped
        else ${ opTree.render(wrapped = false) }
      if (matched) org.parboiled2.Rule.asInstanceOf[Rule[I, O]] else null
    }
  }

  private def reportError(error: String, expr: Expr[Any])(using quotes: Quotes): Nothing = {
    quotes.reflect.report.error(error, expr)
    throw new Exception(error)
  }

  sealed trait OpTree {
    def parser: Expr[Parser]
    def render(wrapped: Boolean)(using Quotes): Expr[Boolean]
  }

  case class Sequence(ops: Seq[OpTree]) extends OpTree {
    val parser = ops.head.parser
    override def render(wrapped: Boolean)(using Quotes): Expr[Boolean] =
      ops
        .map(_.render(wrapped))
        .reduceLeft((l, r) => '{ val ll = $l; if (ll) $r else false })
  }

  sealed abstract class TerminalOpTree extends OpTree {
    def bubbleUp(using quotes: Quotes): Expr[Nothing] = '{ $parser.__bubbleUp($ruleTraceTerminal) }
    def ruleTraceTerminal(using quotes: Quotes): Expr[RuleTrace.Terminal]

    final def render(wrapped: Boolean)(using quotes: Quotes): Expr[Boolean] =
      if (wrapped) '{
        try ${ renderInner(wrapped) } catch { case org.parboiled2.Parser.StartTracingException => $bubbleUp }
      }
      else renderInner(wrapped)

    protected def renderInner(wrapped: Boolean)(using Quotes): Expr[Boolean]
  }

  sealed abstract class PotentiallyNamedTerminalOpTree extends TerminalOpTree {
    // TODO
  }

  case class CharMatch(parser: Expr[Parser], charTree: Expr[Char]) extends TerminalOpTree {
    def ruleTraceTerminal(using quotes: Quotes) = '{ org.parboiled2.RuleTrace.CharMatch($charTree) }
    override def renderInner(wrapped: Boolean)(using Quotes): Expr[Boolean] = {
      val unwrappedTree = '{
        $parser.cursorChar == $charTree && $parser.__advance()
      }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }

  case class StringMatch(parser: Expr[Parser], stringTree: Expr[String]) extends OpTree {
    final private val autoExpandMaxStringLength = 8

    override def render(wrapped: Boolean)(using Quotes): Expr[Boolean] =
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

  case class CharPredicateMatch(parser: Expr[Parser], predicateTree: Expr[CharPredicate])
      extends PotentiallyNamedTerminalOpTree {
    def ruleTraceTerminal(using quotes: Quotes) = '{ org.parboiled2.RuleTrace.CharPredicateMatch($predicateTree) }

    override def renderInner(wrapped: Boolean)(using Quotes): Expr[Boolean] = {
      val unwrappedTree = '{ $predicateTree($parser.cursorChar) && $parser.__advance() }
      if (wrapped) '{ $unwrappedTree && $parser.__updateMaxCursor() || $parser.__registerMismatch() }
      else unwrappedTree
    }
  }
}
