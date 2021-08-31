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

import scala.annotation.tailrec

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
  import quotes.reflect.*

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
  sealed abstract private class PotentiallyNamedTerminalOpTree(arg: Term) extends TerminalOpTree {
    override def bubbleUp: Expr[Nothing] =
      callName(arg) match {
        case Some(name) =>
          '{ $parser.__bubbleUp(RuleTrace.NonTerminal(RuleTrace.Named(${ Expr(name) }), 0) :: Nil, $ruleTraceTerminal) }
        case None => super.bubbleUp
      }
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

  def FirstOf(lhs: OpTree, rhs: OpTree): FirstOf =
    lhs -> rhs match {
      case (FirstOf(lops), FirstOf(rops)) => FirstOf(lops ++ rops)
      case (FirstOf(lops), _)             => FirstOf(lops :+ rhs)
      case (_, FirstOf(ops))              => FirstOf(lhs +: ops)
      case _                              => FirstOf(Seq(lhs, rhs))
    }
  case class FirstOf(ops: Seq[OpTree]) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = '{ RuleTrace.FirstOf }

    def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean] =
      '{
        val mark = $parser.__saveState
        ${
          ops
            .map(_.render(wrapped))
            .reduceLeft((l0, r) =>
              '{
                val l = $l0
                if (!l) {
                  $parser.__restoreState(mark)
                  $r
                } else
                  true // work-around for https://issues.scala-lang.org/browse/SI-8657", FIXME: still valid for dotty?
              }
            )
        }
      }
  }

  sealed abstract class WithSeparator extends DefaultNonTerminalOpTree {
    def withSeparator(sep: Separator): OpTree
  }

  case class ZeroOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    def ruleTraceNonTerminalKey       = '{ RuleTrace.ZeroOrMore }

    def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean] =
      collector.withCollector { coll =>
        '{
          @ _root_.scala.annotation.tailrec
          def rec(mark: org.parboiled2.Parser.Mark): org.parboiled2.Parser.Mark = {
            val matched = ${ op.render(wrapped) }
            if (matched) {
              ${ coll.popToBuilder }
              ${
                if (separator eq null) '{ rec($parser.__saveState) }
                else
                  '{
                    val m = $parser.__saveState
                    if (${ separator(wrapped) }) rec(m) else m
                  }
              }
            } else mark
          }

          $parser.__restoreState(rec($parser.__saveState))
          ${ coll.pushBuilderResult }
          true
        }
      }
  }
  case class OneOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    def ruleTraceNonTerminalKey       = '{ RuleTrace.OneOrMore }

    def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean] =
      collector.withCollector { coll =>
        '{
          @ _root_.scala.annotation.tailrec
          def rec(mark: org.parboiled2.Parser.Mark): org.parboiled2.Parser.Mark = {
            val matched = ${ op.render(wrapped) }
            if (matched) {
              ${ coll.popToBuilder }
              ${
                if (separator eq null) '{ rec($parser.__saveState) }
                else
                  '{
                    val m = $parser.__saveState
                    if (${ separator(wrapped) }) rec(m) else m
                  }
              }
            } else mark
          }

          val firstMark = $parser.__saveState
          val mark      = rec(firstMark)
          mark != firstMark && { // FIXME: almost the same as ZeroOrMore and should be combined
            $parser.__restoreState(mark)
            ${ coll.pushBuilderResult }
            true
          }
        }
      }
  }

  private case class Action(body: Term, ts: List[TypeTree]) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = '{ RuleTrace.Action }

    def popToVals(valdefs: List[ValDef]): List[Statement] = {
      def convertOne(v: ValDef): ValDef =
        v.tpt.tpe.asType match {
          case '[t] => ValDef.copy(v)(v.name, v.tpt, Some('{ $parser.valueStack.pop().asInstanceOf[t] }.asTerm))
        }

      valdefs.map(convertOne).reverse
    }

    def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean] = {
      body match {
        case Lambda(args, body) =>
          def rewrite(tree: Term): Term =
            tree match {
              case Block(statements, res) => block(statements, rewrite(res))
              //case x if isSubClass(actionType.last, "org.parboiled2.Rule") => expand(x, wrapped)
              case x => '{ $parser.__push(${ x.asExpr }) }.asTerm
            }
          // do a beta reduction, using the parameter definitions as stubs for variables
          // that hold values popped from the stack
          block(popToVals(args), rewrite(body)).asExprOf[Boolean]
      }
    }

    /*val actionType: List[Type] = actionTypeTree.tpe match {
      case TypeRef(_, _, args) if args.nonEmpty => args
      case x                                    => c.abort(actionTree.pos, "Unexpected action type: " + x)
    }
    def ruleTraceNonTerminalKey = reify(RuleTrace.Action).tree

    def renderInner(wrapped: Boolean): Tree = {
      val argTypes = actionType dropRight 1

      def popToVals(valNames: List[TermName]): List[Tree] =
        (valNames zip argTypes).map { case (n, t) => q"val $n = valueStack.pop().asInstanceOf[$t]" }.reverse

      def actionBody(tree: Tree): Tree =
        tree match {
          case Block(statements, res) => block(statements, actionBody(res))

          case x @ (Ident(_) | Select(_, _)) =>
            val valNames = argTypes.indices.map(i => TermName("value" + i)).toList
            val args     = valNames map Ident.apply
            block(popToVals(valNames), q"__push($x(..$args))")

          case q"(..$args => $body)" =>
            def rewrite(tree: Tree): Tree =
              tree match {
                case Block(statements, res)                                  => block(statements, rewrite(res))
                case x if isSubClass(actionType.last, "org.parboiled2.Rule") => expand(x, wrapped)
                case x                                                       => q"__push($x)"
              }
            block(popToVals(args.map(_.name)), rewrite(body))
        }

      actionBody(c.untypecheck(actionTree))
    }*/
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

  private case class PushAction(valueExpr: Expr[_], argType: Type[_]) extends OpTree {
    def render(wrapped: Boolean): Expr[Boolean] = {
      val body =
        argType match {
          case '[Unit]  => valueExpr
          case '[HList] => '{ $parser.valueStack.pushAll($valueExpr.asInstanceOf[HList]) }
          case _        => '{ $parser.valueStack.push($valueExpr) }
        }

      '{
        $body
        true
      }
    }
  }
  private case class DropAction(tpe: Type[_]) extends OpTree {
    def render(wrapped: Boolean): Expr[Boolean] = {
      import support.hlist._
      val body =
        tpe match {
          case '[Unit] => '{}
          case '[HList] =>
            @tailrec def rec(t: Type[_], prefix: Expr[Unit]): Expr[Unit] = t match {
              case '[HNil] => prefix
              case '[h :: t] =>
                rec(Type.of[t], '{ $prefix; $parser.valueStack.pop() })

            }
            rec(tpe, '{})

          case _ => '{ $parser.valueStack.pop() }
        }

      '{
        $body
        true
      }
    }
  }

  private case class RuleCall(call: Either[OpTree, Expr[Rule[_, _]]], calleeNameTree: Expr[String])
      extends NonTerminalOpTree {

    def bubbleUp(e: Expr[Parser#TracingBubbleException], start: Expr[Int]): Expr[Nothing] =
      '{ $e.prepend(RuleTrace.RuleCall, $start).bubbleUp(RuleTrace.Named($calleeNameTree), $start) }

    override def render(wrapped: Boolean): Expr[Boolean] = call match {
      case Left(_)     => super.render(wrapped)
      case Right(rule) => '{ $rule ne null }
    }
    protected def renderInner(start: Expr[Int], wrapped: Boolean): Expr[Boolean] = {
      val Left(value) = call
      value.render(wrapped)
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

  private case class CharPredicateMatch(predicateTree: Expr[CharPredicate])
      extends PotentiallyNamedTerminalOpTree(predicateTree.asTerm) {
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
          s"unknown rule: [${${ Expr(syntax) }}] '${${ Expr(tree) }}' in [${${ Expr(outerSyntax) }}]"
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

  def topLevel(opTree: OpTree, name: Expr[String]): OpTree = RuleCall(Left(opTree), name)

  def deconstruct(outerRule: Expr[Rule[_, _]]): OpTree = {
    import quotes.reflect.*

    def collector(lifter: Term): Collector =
      lifter match {
        case TypeApply(Ident("forRule0" | "forReduction"), _) => rule0Collector
        case TypeApply(Ident("forRule1"), _)                  => rule1Collector
        case x                                                => reportError(s"Unexpected lifter ${lifter.show(using Printer.TreeStructure)}", lifter.asExpr)
      }

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
      case '{ ($p: Parser).push[t]($value) }                     => PushAction(value, Type.of[t])
      case '{ ($p: Parser).drop[t] }                             => DropAction(Type.of[t])
      case x                                                     =>
        // These patterns cannot be parsed as quoted patterns because of the complicated type applies
        x.asTerm.underlyingArgument match {
          case Apply(Apply(TypeApply(Select(lhs, "~"), _), List(rhs)), _) => Sequence(Seq(rec(lhs), rec(rhs)))
          case Apply(TypeApply(Select(lhs, "|"), _), List(rhs))           => FirstOf(rec(lhs), rec(rhs))

          case Apply(Apply(TypeApply(Select(_, "zeroOrMore"), _), List(arg)), List(l)) =>
            ZeroOrMore(rec(arg), collector(l))
          case Apply(Select(base, "*"), List(l)) => ZeroOrMore(rec(base), collector(l))

          case Apply(
                Select(
                  Apply(
                    TypeApply(Select(_, "rule2WithSeparatedBy"), _),
                    List(Apply(Apply(TypeApply(Select(_, "zeroOrMore"), _), List(base)), List(l)))
                  ),
                  "separatedBy"
                ),
                List(sep)
              ) =>
            ZeroOrMore(rec(base), collector(l), Separator(rec(sep)))
          case Apply(Apply(Select(base, "*"), List(sep)), List(l)) =>
            ZeroOrMore(rec(base), collector(l), Separator(rec(sep)))

          case Apply(Apply(TypeApply(Select(_, "oneOrMore"), _), List(arg)), List(l)) =>
            OneOrMore(rec(arg), collector(l))
          case Apply(Select(base, "+"), List(l)) => OneOrMore(rec(base), collector(l))

          case Apply(
                Apply(
                  TypeApply(
                    Select(
                      Select(Apply(Apply(TypeApply(Select(_, "rule2ActionOperator"), _), List(base)), _), "~>"),
                      "apply"
                    ),
                    _
                  ),
                  List(body)
                ),
                List(_, TypeApply(Ident("apply"), ts))
              ) =>
            Sequence(rec(base), Action(body, ts))
          case Apply(
                Select(
                  Apply(
                    TypeApply(Select(_, "rule2WithSeparatedBy"), _),
                    List(Apply(Apply(TypeApply(Select(_, "oneOrMore"), _), List(base)), List(l)))
                  ),
                  "separatedBy"
                ),
                List(sep)
              ) =>
            OneOrMore(rec(base), collector(l), Separator(rec(sep)))
          case Apply(Apply(Select(base, "+"), List(sep)), List(l)) =>
            OneOrMore(rec(base), collector(l), Separator(rec(sep)))

          case Apply(Apply(TypeApply(Select(_, "capture"), _), List(arg)), _) => Capture(rec(arg))
          case call @ (Apply(_, _) | Select(_, _) | Ident(_) | TypeApply(_, _)) =>
            RuleCall(
              Right(call.asExprOf[Rule[_, _]]),
              Expr(callName(call) getOrElse reportError("Illegal rule call: " + call, call.asExpr))
            )
          case _ => Unknown(rule.show, rule.show(using Printer.TreeStructure), outerRule.toString)
        }
    }

    rec(outerRule.asTerm)
  }

  private def reportError(error: String, expr: Expr[Any]): Nothing = {
    quotes.reflect.report.error(error, expr)
    throw new scala.quoted.runtime.StopMacroExpansion
  }

  /////////////////////////////////// helpers ////////////////////////////////////

  trait Collector {
    def withCollector(f: CollectorInstance => Expr[Boolean]): Expr[Boolean]
  }
  trait CollectorInstance {
    def popToBuilder: Expr[Unit]
    def pushBuilderResult: Expr[Unit]
  }

  // no-op collector
  object rule0Collector extends Collector with CollectorInstance {
    override def withCollector(f: CollectorInstance => Expr[Boolean]): Expr[Boolean] = f(this)
    val popToBuilder: Expr[Unit]                                                     = '{}
    val pushBuilderResult: Expr[Unit]                                                = '{}
  }

  object rule1Collector extends Collector {
    override def withCollector(f: CollectorInstance => Expr[Boolean]): Expr[Boolean] = '{
      val builder = new scala.collection.immutable.VectorBuilder[Any]
      ${
        f(new CollectorInstance {
          override def popToBuilder: Expr[Unit]      = '{ builder += $parser.valueStack.pop() }
          override def pushBuilderResult: Expr[Unit] = '{ $parser.valueStack.push(builder.result()) }
        })
      }
    }
  }

  type Separator = Boolean => Expr[Boolean]
  private def Separator(op: OpTree): Separator = wrapped => op.render(wrapped)

  @tailrec
  private def callName(tree: Term): Option[String] =
    tree match {
      case Ident(name)       => Some(name)
      case Select(_, name)   => Some(name)
      case Apply(fun, _)     => callName(fun)
      case TypeApply(fun, _) => callName(fun)
      case _                 => None
    }

  // tries to match and expand the leaves of the given Tree
  private def expand(tree: Tree, wrapped: Boolean): Tree =
    tree match {
      case Block(statements, res) => block(statements, expand(res, wrapped).asInstanceOf[Term])
      case If(cond, thenExp, elseExp) =>
        If(cond, expand(thenExp, wrapped).asInstanceOf[Term], expand(elseExp, wrapped).asInstanceOf[Term])
      case Match(selector, cases)    => Match(selector, cases.map(expand(_, wrapped).asInstanceOf[CaseDef]))
      case CaseDef(pat, guard, body) => CaseDef(pat, guard, expand(body, wrapped).asInstanceOf[Term])
      case x                         => tree //??? //opTreePF.andThen(_.render(wrapped)).applyOrElse(tree, (t: Tree) => q"$t ne null")
    }

  private def block(a: Term, b: Term): Term =
    a match {
      case Block(a1, a2) =>
        b match {
          case Block(b1, b2) => Block(a1 ::: a2 :: b1, b2)
          case _             => Block(a1 ::: a2 :: Nil, b)
        }
      case _ =>
        b match {
          case Block(b1, b2) => Block(a :: b1, b2)
          case _             => Block(a :: Nil, b)
        }
    }

  private def block(stmts: List[Statement], expr: Term): Term =
    expr match {
      case Block(a, b) => block(stmts ::: a ::: Nil, b)
      case _           => Block(stmts, expr)
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
    val opTree = ctx.topLevel(ctx.deconstruct(r), name)

    '{
      def wrapped: Boolean = ${ opTree.render(wrapped = true) }
      val matched =
        if ($parser.__inErrorAnalysis) wrapped
        else ${ opTree.render(wrapped = false) }
      if (matched) org.parboiled2.Rule.asInstanceOf[Rule[I, O]] else null
    }
  }
}
