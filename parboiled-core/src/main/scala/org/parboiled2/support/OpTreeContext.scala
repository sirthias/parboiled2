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

package org.parboiled2.support

import scala.annotation.tailrec
import org.parboiled2._

class OpTreeContext[OpTreeCtx <: reflect.macros.blackbox.Context](val c: OpTreeCtx) {
  import c.universe._

  var tpeCtx: c.Type = _

  sealed trait OpTree {
    // renders a Boolean Tree
    def render(wrapped: Boolean): Tree
  }

  sealed abstract class NonTerminalOpTree extends OpTree {
    def bubbleUp: Tree

    // renders a Boolean Tree
    def render(wrapped: Boolean): Tree =
      if (wrapped) q"""
        val start = __psi.cursor
        try ${renderInner(wrapped)}
        catch { case e: $prefix.ParserStateImpl[_]#TracingBubbleException ⇒ $bubbleUp }"""
      else renderInner(wrapped)

    // renders a Boolean Tree
    protected def renderInner(wrapped: Boolean): Tree
  }

  sealed abstract class DefaultNonTerminalOpTree extends NonTerminalOpTree {
    def bubbleUp: Tree = q"e.bubbleUp($ruleTraceNonTerminalKey, start)"
    def ruleTraceNonTerminalKey: Tree
  }

  sealed abstract class TerminalOpTree extends OpTree {
    def bubbleUp: Tree = q"__psi.bubbleUp($ruleTraceTerminal)"
    def ruleTraceTerminal: Tree

    // renders a Boolean Tree
    final def render(wrapped: Boolean): Tree =
      if (wrapped) q"""
        try ${renderInner(wrapped)}
        catch { case $prefix.ParserStateImpl.StartTracingException ⇒ $bubbleUp }"""
      else renderInner(wrapped)

    // renders a Boolean Tree
    protected def renderInner(wrapped: Boolean): Tree
  }

  sealed abstract class PotentiallyNamedTerminalOpTree(arg: Tree) extends TerminalOpTree {
    override def bubbleUp = callName(arg) match {
      case Some(name) ⇒ q"__psi.bubbleUp($prefix.RuleTrace.NonTerminal($prefix.RuleTrace.Named($name), 0) :: Nil, $ruleTraceTerminal)"
      case None       ⇒ super.bubbleUp
    }
    def ruleTraceTerminal: Tree
  }

  def collector(lifterTree: Tree): Collector =
    lifterTree match {
      case q"support.this.$a.forRule0[$b]" ⇒ rule0Collector
      case q"support.this.$a.forRule1[$b, $c]" ⇒ rule1Collector
      case q"support.this.$a.forReduction[$b, $c, $d]" ⇒ rule0Collector
      case x ⇒ c.abort(x.pos, "Unexpected Lifter: " + lifterTree)
    }

  val opTreePF: PartialFunction[Tree, OpTree] = {
    case q"$lhs.~[..$a]($rhs)($c, $d)"                   ⇒ Sequence(OpTree(lhs), OpTree(rhs))
    case q"$lhs.~!~[..$a]($rhs)($c, $d)"                 ⇒ Cut(OpTree(lhs), OpTree(rhs))
    case q"$lhs.|[..$a]($rhs)($b)"                       ⇒ FirstOf(OpTree(lhs), OpTree(rhs))
    case q"$a.this.ch($c)"                               ⇒ CharMatch(StateAccessTransformer(c))
    case q"$a.this.str($s)"                              ⇒ StringMatch(StateAccessTransformer(s))
    case q"$a.this.valueMap[$b]($m)($hl)"                ⇒ MapMatch(StateAccessTransformer(m))
    case q"$a.this.ignoreCase($t)"                       ⇒ IgnoreCase(t)
    case q"$a.this.predicate($p)"                        ⇒ CharPredicateMatch(StateAccessTransformer(p))
    case q"$a.this.anyOf($s)"                            ⇒ AnyOf(StateAccessTransformer(s))
    case q"$a.this.noneOf($s)"                           ⇒ NoneOf(StateAccessTransformer(s))
    case q"$a.this.ANY"                                  ⇒ ANY
    case q"$a.this.MATCH"                                ⇒ MATCH
    case q"$a.this.MISMATCH0"                            ⇒ MISMATCH
    case q"$a.this.MISMATCH[..$t]"                       ⇒ MISMATCH
    case q"$a.this.optional[..$b]($arg)($l)"             ⇒ Optional(OpTree(arg), collector(l))
    case q"$base.?($l)"                                  ⇒ Optional(OpTree(base), collector(l))
    case q"$a.this.zeroOrMore[..$b]($arg)($l)"           ⇒ ZeroOrMore(OpTree(arg), collector(l))
    case q"$base.*($l)"                                  ⇒ ZeroOrMore(OpTree(base), collector(l))
    case q"$base.*[$a]($sep)($l)"                        ⇒ ZeroOrMore(OpTree(base), collector(l), Separator(OpTree(sep)))
    case q"$a.this.oneOrMore[..$b]($arg)($l)"            ⇒ OneOrMore(OpTree(arg), collector(l))
    case q"$base.+($l)"                                  ⇒ OneOrMore(OpTree(base), collector(l))
    case q"$base.+[$a]($sep)($l)"                        ⇒ OneOrMore(OpTree(base), collector(l), Separator(OpTree(sep)))
    case q"$base.times[..$a]($r)($s)"                    ⇒ Times(base, OpTree(r), collector(s))
    case q"$a.this.&[$b]($arg)"                          ⇒ AndPredicate(OpTree(arg))
    case q"$a.unary_!()"                                 ⇒ NotPredicate(OpTree(a))
    case q"$a.this.atomic[..$b]($arg)"                   ⇒ Atomic(OpTree(arg))
    case q"$a.this.quiet[..$b]($arg)"                    ⇒ Quiet(OpTree(arg))
    case q"$a.this.test($arg)"                           ⇒ SemanticPredicate(StateAccessTransformer(arg))
    case q"$a.this.capture[..$b]($arg)($d)"              ⇒ Capture(OpTree(arg))
    case q"$a.this.capturePos[..$b]($arg)($d)"           ⇒ CapturePos(OpTree(arg))
    case q"$a.this.run[$b]($arg)($e.fromAux[..$d]($rr))" ⇒ RunAction(StateAccessTransformer(arg), rr)
    case q"$a.this.push[$b]($arg)($hl)"                  ⇒ PushAction(StateAccessTransformer(arg), hl)
    case q"$a.this.drop[$b]($hl)"                        ⇒ DropAction(hl)
    case q"$a.this.fail($m)"                             ⇒ Fail(StateAccessTransformer(m))
    case q"$a.this.failX[..$b]($m)"                      ⇒ Fail(StateAccessTransformer(m))
    case q"$a.named($name)"                              ⇒ Named(OpTree(a), name)
    case x @ q"$a.this.str2CharRangeSupport($l).-($r)"   ⇒ CharRange(l, r)
    case q"$a.this.charAndValue[$t]($b.ArrowAssoc[$t1]($c).->[$t2]($v))($hl)" ⇒
      Sequence(CharMatch(c), PushAction(v, hl))
    case q"$a.this.stringAndValue[$t]($b.ArrowAssoc[$t1]($s).->[$t2]($v))($hl)" ⇒
      Sequence(StringMatch(s), PushAction(v, hl))
    case q"$a.this.rule2ActionOperator[..$b]($r)($o).~>.apply[..$e]($f)($g, support.this.FCapture.apply[$ts])" ⇒
      Sequence(OpTree(r), Action(StateAccessTransformer(f), ts))
    case x @ q"$a.this.rule2WithSeparatedBy[..$b]($base).separatedBy[$d]($sep)" ⇒
      OpTree(base) match {
        case x: WithSeparator ⇒ x.withSeparator(Separator(OpTree(sep)))
        case _                ⇒ c.abort(x.pos, "Illegal `separatedBy` base: " + base)
      }
    case Apply(Select(target, TermName("apply")), List(arg0, arg1, arg2)) if isSubType(target, Rule3XType) ⇒
      RuleCall(Call3(target, arg0, arg1, arg2), callName(target, "Unexpected Rule3X call: "))
    case Apply(Select(target, TermName("apply")), List(arg0, arg1)) if isSubType(target, Rule2XType) ⇒
      RuleCall(Call2(target, arg0, arg1), callName(target, "Unexpected Rule2X call: "))
    case Apply(Select(target, TermName("apply")), List(arg)) if isSubType(target, Rule1XType) ⇒
      RuleCall(Call1(target, arg), callName(target, "Unexpected Rule1X call: "))
    case call @ (Apply(_, _) | Select(_, _) | Ident(_) | TypeApply(_, _)) ⇒
      RuleCall(Call0(call), callName(call, "Unexpected call: "))
  }

  def OpTree(tree: Tree): OpTree =
    opTreePF.applyOrElse(tree, (t: Tree) ⇒ c.abort(t.pos, "Invalid rule definition: " + t))

  def Sequence(lhs: OpTree, rhs: OpTree): Sequence =
    lhs -> rhs match {
      case (Sequence(lops), Sequence(rops)) ⇒ Sequence(lops ++ rops)
      case (Sequence(lops), _)              ⇒ Sequence(lops :+ rhs)
      case (_, Sequence(ops))               ⇒ Sequence(lhs +: ops)
      case _                                ⇒ Sequence(Seq(lhs, rhs))
    }

  case class Sequence(ops: Seq[OpTree]) extends DefaultNonTerminalOpTree {
    require(ops.size >= 2)
    def ruleTraceNonTerminalKey = reify(RuleTrace.Sequence).tree
    def renderInner(wrapped: Boolean): Tree =
      ops.map(_.render(wrapped)).reduceLeft((l, r) ⇒
        q"val l = $l; if (l) $r else false") // work-around for https://issues.scala-lang.org/browse/SI-8657"
  }

  case class Cut(lhs: OpTree, rhs: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.Cut).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      var matched = ${lhs.render(wrapped)}
      if (matched) {
        matched = ${rhs.render(wrapped)}
        if (!matched) throw $prefix.ParserStateImpl.CutError
        true
      } else false""" // work-around for https://issues.scala-lang.org/browse/SI-8657
  }

  def FirstOf(lhs: OpTree, rhs: OpTree): FirstOf =
    lhs -> rhs match {
      case (FirstOf(lops), FirstOf(rops)) ⇒ FirstOf(lops ++ rops)
      case (FirstOf(lops), _)             ⇒ FirstOf(lops :+ rhs)
      case (_, FirstOf(ops))              ⇒ FirstOf(lhs +: ops)
      case _                              ⇒ FirstOf(Seq(lhs, rhs))
    }

  case class FirstOf(ops: Seq[OpTree]) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.FirstOf).tree
    def renderInner(wrapped: Boolean): Tree =
      q"""val mark = __psi.saveState; ${
        ops.map(_.render(wrapped)).reduceLeft((l, r) ⇒
          q"val l = $l; if (!l) { __psi.restoreState(mark); $r } else true // work-around for https://issues.scala-lang.org/browse/SI-8657")
      }"""
  }

  case class CharMatch(charTree: Tree) extends TerminalOpTree {
    def ruleTraceTerminal = q"$prefix.RuleTrace.CharMatch($charTree)"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"__psi.cursorChar == $charTree && __psi.advance()"
      if (wrapped) q"$unwrappedTree && __psi.updateMaxCursor() || __psi.registerMismatch()" else unwrappedTree
    }
  }

  case class StringMatch(stringTree: Tree) extends OpTree {
    final private val autoExpandMaxStringLength = 8
    def render(wrapped: Boolean): Tree = {
      def unrollUnwrapped(s: String, ix: Int = 0): Tree =
        if (ix < s.length) q"""
          if (__psi.cursorChar == ${s charAt ix}) {
            __psi.advance()
            ${unrollUnwrapped(s, ix + 1)}:Boolean
          } else false"""
        else q"true"
      def unrollWrapped(s: String, ix: Int = 0): Tree =
        if (ix < s.length) {
          val ch = s charAt ix
          q"""if (__psi.cursorChar == $ch) {
            __psi.advance()
            __psi.updateMaxCursor()
            ${unrollWrapped(s, ix + 1)}
          } else {
            try __psi.registerMismatch()
            catch {
              case $prefix.ParserStateImpl.StartTracingException ⇒
                import $prefix.RuleTrace._
                __psi.bubbleUp(NonTerminal(StringMatch($stringTree), -$ix) :: Nil, CharMatch($ch))
            }
          }"""
        } else q"true"

      stringTree match {
        case Literal(Constant(s: String)) if s.length <= autoExpandMaxStringLength ⇒
          if (s.isEmpty) q"true" else if (wrapped) unrollWrapped(s) else unrollUnwrapped(s)
        case _ ⇒
          if (wrapped) q"__psi.matchStringWrapped($stringTree)"
          else q"__psi.matchString($stringTree)"
      }
    }
  }

  case class MapMatch(mapTree: Tree) extends OpTree {
    def render(wrapped: Boolean): Tree = if (wrapped) q"__psi.matchMapWrapped($mapTree)" else q"__psi.matchMap($mapTree)"
  }

  def IgnoreCase(argTree: Tree): OpTree = {
    val argTypeSymbol = argTree.tpe.typeSymbol
    if (argTypeSymbol == definitions.CharClass) IgnoreCaseChar(argTree)
    else if (argTypeSymbol == definitions.StringClass) IgnoreCaseString(argTree)
    else c.abort(argTree.pos, "Unexpected `ignoreCase` argument type: " + argTypeSymbol)
  }

  case class IgnoreCaseChar(charTree: Tree) extends TerminalOpTree {
    def ruleTraceTerminal = q"$prefix.RuleTrace.IgnoreCaseChar($charTree)"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"_root_.java.lang.Character.toLowerCase(__psi.cursorChar) == $charTree && __psi.advance()"
      if (wrapped) q"$unwrappedTree && __psi.updateMaxCursor() || __psi.registerMismatch()" else unwrappedTree
    }
  }

  case class IgnoreCaseString(stringTree: Tree) extends OpTree {
    final private val autoExpandMaxStringLength = 8
    def render(wrapped: Boolean): Tree = {
      def unrollUnwrapped(s: String, ix: Int = 0): Tree =
        if (ix < s.length) q"""
          if (_root_.java.lang.Character.toLowerCase(__psi.cursorChar) == ${s charAt ix}) {
            __psi.advance()
            ${unrollUnwrapped(s, ix + 1)}
          } else false"""
        else q"true"
      def unrollWrapped(s: String, ix: Int = 0): Tree =
        if (ix < s.length) {
          val ch = s charAt ix
          q"""if (_root_.java.lang.Character.toLowerCase(__psi.cursorChar) == $ch) {
            __psi.advance()
            __psi.updateMaxCursor()
            ${unrollWrapped(s, ix + 1)}
          } else {
            try __psi.registerMismatch()
            catch {
              case $prefix.ParserStateImpl.StartTracingException ⇒
                import $prefix.RuleTrace._
                __psi.bubbleUp(NonTerminal(IgnoreCaseString($stringTree), -$ix) :: Nil, IgnoreCaseChar($ch))
            }
          }"""
        } else q"true"

      stringTree match {
        case Literal(Constant(s: String)) if s.length <= autoExpandMaxStringLength ⇒
          if (s.isEmpty) q"true" else if (wrapped) unrollWrapped(s) else unrollUnwrapped(s)
        case _ ⇒
          if (wrapped) q"__psi.matchIgnoreCaseStringWrapped($stringTree)"
          else q"__psi.matchIgnoreCaseString($stringTree)"
      }
    }
  }

  case class CharPredicateMatch(predicateTree: Tree) extends PotentiallyNamedTerminalOpTree(predicateTree) {
    def ruleTraceTerminal = q"$prefix.RuleTrace.CharPredicateMatch($predicateTree)"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"$predicateTree(__psi.cursorChar) && __psi.advance()"
      if (wrapped) q"$unwrappedTree && __psi.updateMaxCursor() || __psi.registerMismatch()" else unwrappedTree
    }
  }

  case class AnyOf(stringTree: Tree) extends TerminalOpTree {
    def ruleTraceTerminal = q"$prefix.RuleTrace.AnyOf($stringTree)"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"__psi.matchAnyOf($stringTree)"
      if (wrapped) q"$unwrappedTree && __psi.updateMaxCursor() || __psi.registerMismatch()" else unwrappedTree
    }
  }

  case class NoneOf(stringTree: Tree) extends TerminalOpTree {
    def ruleTraceTerminal = q"$prefix.RuleTrace.NoneOf($stringTree)"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"__psi.matchNoneOf($stringTree)"
      if (wrapped) q"$unwrappedTree && __psi.updateMaxCursor() || __psi.registerMismatch()" else unwrappedTree
    }
  }

  case object ANY extends TerminalOpTree {
    def ruleTraceTerminal = reify(RuleTrace.ANY).tree
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"__psi.cursorChar != EOI && __psi.advance()"
      if (wrapped) q"$unwrappedTree && __psi.updateMaxCursor() || __psi.registerMismatch()" else unwrappedTree
    }
  }

  case object MATCH extends OpTree {
    def render(wrapped: Boolean) = q"true"
  }

  case object MISMATCH extends TerminalOpTree {
    def ruleTraceTerminal = reify(RuleTrace.MISMATCH).tree
    def renderInner(wrapped: Boolean): Tree = if (wrapped) q"__psi.registerMismatch()" else q"false"
  }

  case class Optional(op: OpTree, collector: Collector) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.Optional).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      val mark = __psi.saveState
      val matched = ${op.render(wrapped)}
      if (matched) {
        ${collector.pushSomePop}
      } else {
        __psi.restoreState(mark)
        ${collector.pushNone}
      }
      true"""
  }

  sealed abstract class WithSeparator extends DefaultNonTerminalOpTree {
    def withSeparator(sep: Separator): OpTree
  }

  case class ZeroOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    def ruleTraceNonTerminalKey = reify(RuleTrace.ZeroOrMore).tree
    def renderInner(wrapped: Boolean): Tree = {
      val recurse =
        if (separator eq null) q"rec(__psi.saveState)"
        else q"val m = __psi.saveState; if (${separator(wrapped)}) rec(m) else m"

      q"""
      ${collector.valBuilder}

      @_root_.scala.annotation.tailrec def rec(mark: $prefix.ParserStateImpl.Mark): $prefix.ParserStateImpl.Mark = {
        val matched = ${op.render(wrapped)}
        if (matched) {
          ${collector.popToBuilder}
          $recurse
        } else mark
      }

      __psi.restoreState(rec(__psi.saveState))
      ${collector.pushBuilderResult}"""
    }
  }

  case class OneOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    def ruleTraceNonTerminalKey = reify(RuleTrace.OneOrMore).tree
    def renderInner(wrapped: Boolean): Tree = {
      val recurse =
        if (separator eq null) q"rec(__psi.saveState)"
        else q"val m = __psi.saveState; if (${separator(wrapped)}) rec(m) else m"

      q"""
      val firstMark = __psi.saveState
      ${collector.valBuilder}

      @_root_.scala.annotation.tailrec def rec(mark: $prefix.ParserStateImpl.Mark): $prefix.ParserStateImpl.Mark = {
        val matched = ${op.render(wrapped)}
        if (matched) {
          ${collector.popToBuilder}
          $recurse
        } else mark
      }

      val mark = rec(firstMark)
      mark != firstMark && {
        __psi.restoreState(mark)
        ${collector.pushBuilderResult}
      }"""
    }
  }

  def Times(base: Tree, rule: OpTree, collector: Collector, separator: Separator = null): OpTree =
    base match {
      case q"$a.this.int2NTimes($n)" ⇒ n match {
        case Literal(Constant(i: Int)) ⇒
          if (i <= 0) c.abort(base.pos, "`x` in `x.times` must be positive")
          else if (i == 1) rule
          else Times(rule, q"val min, max = $n", collector, separator)
        case x @ (Ident(_) | Select(_, _)) ⇒ Times(rule, q"val min = $n; val max = min", collector, separator)
        case _                             ⇒ c.abort(n.pos, "Invalid int base expression for `.times(...)`: " + n)
      }
      case q"$a.this.range2NTimes($r)" ⇒ r match {
        case q"scala.this.Predef.intWrapper($mn).to($mx)" ⇒ (mn, mx) match {
          case (Literal(Constant(min: Int)), Literal(Constant(max: Int))) ⇒
            if (min <= 0) c.abort(mn.pos, "`min` in `(min to max).times` must be positive")
            else if (max <= 0) c.abort(mx.pos, "`max` in `(min to max).times` must be positive")
            else if (max < min) c.abort(mx.pos, "`max` in `(min to max).times` must be >= `min`")
            else Times(rule, q"val min = $mn; val max = $mx", collector, separator)
          case ((Ident(_) | Select(_, _)), (Ident(_) | Select(_, _))) ⇒
            Times(rule, q"val min = $mn; val max = $mx", collector, separator)
          case _ ⇒ c.abort(r.pos, "Invalid int range expression for `.times(...)`: " + r)
        }
        case x @ (Ident(_) | Select(_, _)) ⇒
          Times(rule, q"val r = $r; val min = r.start; val max = r.end", collector, separator)
        case _ ⇒ c.abort(r.pos, "Invalid range base expression for `.times(...)`: " + r)
      }
      case _ ⇒ c.abort(base.pos, "Invalid base expression for `.times(...)`: " + base)
    }

  case class Times(op: OpTree, init: Tree, collector: Collector, separator: Separator) extends WithSeparator {
    def withSeparator(sep: Separator) = copy(separator = sep)
    val Block(inits, _) = init
    def ruleTraceNonTerminalKey = q"..$inits; $prefix.RuleTrace.Times(min, max)"
    def renderInner(wrapped: Boolean): Tree = {
      val recurse =
        if (separator eq null) q"rec(count + 1, __psi.saveState)"
        else q"""
          val m = __psi.saveState; if (${separator(wrapped)}) rec(count + 1, m)
          else (count >= min) && { __psi.restoreState(m); true }"""

      q"""
      ${collector.valBuilder}
      ..$inits

      @_root_.scala.annotation.tailrec def rec(count: Int, mark: $prefix.ParserStateImpl.Mark): Boolean = {
        val matched = ${op.render(wrapped)}
        if (matched) {
          ${collector.popToBuilder}
          if (count < max) $recurse else true
        } else (count > min) && { __psi.restoreState(mark); true }
      }

      (max <= 0) || rec(1, __psi.saveState) && ${collector.pushBuilderResult}"""
    }
  }

  case class AndPredicate(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.AndPredicate).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      val mark = __psi.saveState
      val matched = ${op.render(wrapped)}
      __psi.restoreState(mark)
      matched"""
  }

  case class NotPredicate(op: OpTree) extends OpTree {
    def render(wrapped: Boolean): Tree = {
      val unwrappedTree = q"""
        val mark = __psi.saveState
        val saved = __psi.enterNotPredicate()
        val matched = ${op.render(wrapped)}
        __psi.exitNotPredicate(saved)
        ${if (wrapped) q"matchEnd = __psi.cursor" else q"()"}
        __psi.restoreState(mark)
        !matched"""
      if (wrapped) {
        val base = op match {
          case x: TerminalOpTree   ⇒ q"$prefix.RuleTrace.NotPredicate.Terminal(${x.ruleTraceTerminal})"
          case x: RuleCall         ⇒ q"$prefix.RuleTrace.NotPredicate.RuleCall(${x.calleeNameTree})"
          case x: StringMatch      ⇒ q"""$prefix.RuleTrace.NotPredicate.Named('"' + ${x.stringTree} + '"')"""
          case x: IgnoreCaseString ⇒ q"""$prefix.RuleTrace.NotPredicate.Named('"' + ${x.stringTree} + '"')"""
          case x: Named            ⇒ q"$prefix.RuleTrace.NotPredicate.Named(${x.stringTree})"
          case _                   ⇒ q"$prefix.RuleTrace.NotPredicate.Anonymous"
        }
        q"""
        var matchEnd = 0
        try $unwrappedTree || __psi.registerMismatch()
        catch {
          case $prefix.ParserStateImpl.StartTracingException ⇒ __psi.bubbleUp {
            $prefix.RuleTrace.NotPredicate($base, matchEnd - __psi.cursor)
          }
        }"""
      } else unwrappedTree
    }
  }

  case class Atomic(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.Atomic).tree
    def renderInner(wrapped: Boolean): Tree =
      if (wrapped) q"""
        val saved = __psi.enterAtomic(start)
        val matched = ${op.render(wrapped)}
        __psi.exitAtomic(saved)
        matched"""
      else op.render(wrapped)
  }

  case class Quiet(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.Quiet).tree
    def renderInner(wrapped: Boolean): Tree =
      if (wrapped) q"""
        val saved = __psi.enterQuiet()
        val matched = ${op.render(wrapped)}
        __psi.exitQuiet(saved)
        matched"""
      else op.render(wrapped)
  }

  case class SemanticPredicate(flagTree: Tree) extends TerminalOpTree {
    def ruleTraceTerminal = reify(RuleTrace.SemanticPredicate).tree
    def renderInner(wrapped: Boolean): Tree =
      if (wrapped) q"$flagTree || __psi.registerMismatch()" else flagTree
  }

  case class Capture(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.Capture).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      ${if (!wrapped) q"val start = __psi.cursor" else q"();"}
      val matched = ${op.render(wrapped)}
      if (matched) {
        __psi.valueStack.push(__psi.input.sliceString(start, __psi.cursor))
        true
      } else false"""
  }

  case class CapturePos(op: OpTree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.CapturePos).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      ${if (!wrapped) q"val start = __psi.cursor" else q"();"}
      val matched = ${op.render(wrapped)}
      if (matched) {
        __psi.valueStack.push($prefix.CapturePos(start, __psi.cursor))
        true
      } else false"""
  }

  case class RunAction(argTree: Tree, rrTree: Tree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = reify(RuleTrace.Run).tree
    def renderInner(wrapped: Boolean): Tree = {
      def renderFunctionAction(resultTypeTree: Tree, argTypeTrees: Tree*): Tree = {
        def actionBody(tree: Tree): Tree =
          tree match {
            case Block(statements, res) ⇒ block(statements, actionBody(res))

            case q"(..$args ⇒ $body)" ⇒
              def rewrite(tree: Tree): Tree =
                tree match {
                  case Block(statements, res)                   ⇒ block(statements, rewrite(res))
                  case x if isSubType(resultTypeTree, RuleType) ⇒ expand(x, wrapped)
                  case x                                        ⇒ q"__psi.push($x)"
                }
              val valDefs = args.zip(argTypeTrees).map { case (a, t) ⇒ q"val ${a.name} = __psi.valueStack.pop().asInstanceOf[${t.tpe}]" }.reverse
              block(valDefs, rewrite(body))

            case x ⇒ c.abort(argTree.pos, "Unexpected `run` argument: " + show(argTree))
          }

        actionBody(argTree)
      }

      rrTree match {
        case q"RunResult.this.Aux.forAny[..$t]"                              ⇒ block(argTree, q"true")

        case q"RunResult.this.Aux.forRule[..$t]"                             ⇒ expand(argTree, wrapped)

        case q"RunResult.this.Aux.forF1[$c, $z, $r, $o]($a)"                 ⇒ renderFunctionAction(r, z)
        case q"RunResult.this.Aux.forF2[$c, $y, $z, $r, $o]($a)"             ⇒ renderFunctionAction(r, y, z)
        case q"RunResult.this.Aux.forF3[$c, $x, $y, $z, $r, $o]($a)"         ⇒ renderFunctionAction(r, x, y, z)
        case q"RunResult.this.Aux.forF4[$c, $w, $x, $y, $z, $r, $o]($a)"     ⇒ renderFunctionAction(r, w, x, y, z)
        case q"RunResult.this.Aux.forF5[$c, $v, $w, $x, $y, $z, $r, $o]($a)" ⇒ renderFunctionAction(r, v, w, x, y, z)

        case q"RunResult.this.Aux.forFHList[$il, $r, $in, $out]($a)" ⇒
          c.abort(argTree.pos, "`run` with a function taking an HList is not yet implemented") // TODO: implement

        case x ⇒ c.abort(rrTree.pos, "Unexpected RunResult.Aux: " + show(x))
      }
    }
  }

  case class PushAction(argTree: Tree, hlTree: Tree) extends OpTree {
    def render(wrapped: Boolean): Tree =
      block(hlTree match {
        case q"support.this.HListable.fromUnit"       ⇒ argTree
        case q"support.this.HListable.fromHList[$t]"  ⇒ q"__psi.valueStack.pushAll($argTree)"
        case q"support.this.HListable.fromAnyRef[$t]" ⇒ q"__psi.valueStack.push($argTree)"
        case x                                        ⇒ c.abort(hlTree.pos, "Unexpected HListable: " + show(x))
      }, q"true")
  }

  case class DropAction(hlTree: Tree) extends OpTree {
    def render(wrapped: Boolean): Tree =
      hlTree match {
        case q"support.this.HListable.fromUnit"       ⇒ q"true"
        case q"support.this.HListable.fromAnyRef[$t]" ⇒ q"__psi.valueStack.pop(); true"
        case q"support.this.HListable.fromHList[$t]" ⇒
          @tailrec def rec(t: Type, result: List[Tree] = Nil): List[Tree] =
            t match { // TODO: how can we use type quotes here, e.g. tq"shapeless.HNil"?
              case TypeRef(_, sym, List(_, tail)) if sym == HListConsClassSymbol ⇒ rec(tail, q"__psi.valueStack.pop()" :: result)
              case TypeRef(_, sym, _) if sym == HNilClassSymbol                  ⇒ result
            }
          Block(rec(t.tpe), q"true")
        case x ⇒ c.abort(hlTree.pos, "Unexpected HListable: " + show(x))
      }
  }

  sealed trait CallType
  final case class OpTreeCall(t: OpTree) extends CallType
  final case class Call0(t: Tree) extends CallType
  final case class Call1(t: Tree, arg: Tree) extends CallType
  final case class Call2(t: Tree, arg0: Tree, arg1: Tree) extends CallType
  final case class Call3(t: Tree, arg0: Tree, arg1: Tree, arg2: Tree) extends CallType

  case class RuleCall(call: CallType, calleeNameTree: Tree) extends NonTerminalOpTree {
    def bubbleUp = q"""
      import $prefix.RuleTrace._
      e.prepend(RuleCall, start).bubbleUp(Named($calleeNameTree), start)"""
    override def render(wrapped: Boolean) =
      call match {
        case OpTreeCall(_)        ⇒ super.render(wrapped)
        case Call0(t)             ⇒ q"$t.asInstanceOf[$prefix.RuleImpl[$tpeCtx]].run(__psi)"
        case Call1(t, a)          ⇒ q"$t.asInstanceOf[${tpe(t, Rule1XType, 1)}].run(${sat(a)}, __psi)"
        case Call2(t, a0, a1)     ⇒ q"$t.asInstanceOf[${tpe(t, Rule2XType, 2)}].run(${sat(a0)}, ${sat(a1)}, __psi)"
        case Call3(t, a0, a1, a2) ⇒ q"$t.asInstanceOf[${tpe(t, Rule3XType, 3)}].run(${sat(a0)}, ${sat(a1)}, ${sat(a2)}, __psi)"
      }
    private def sat = StateAccessTransformer
    private def tpe(t: Tree, ruleNXType: Type, typeParamCount: Int): Tree = {
      val classSymbol = ruleNXType.typeSymbol.asClass
      val typeParams = (1 to typeParamCount).toList.map {
        classSymbol.typeParams(_).asType.toType.asSeenFrom(t.tpe, classSymbol)
      }
      tq"$prefix.${TypeName(classSymbol.name + "Impl")}[..${tpeCtx :: typeParams}]"
    }
    def renderInner(wrapped: Boolean) = call.asInstanceOf[OpTreeCall].t.render(wrapped)
  }

  def CharRange(lowerTree: Tree, upperTree: Tree): CharacterRange = {
    val (lower, upper) = lowerTree -> upperTree match {
      case (Literal(Constant(l: String)), Literal(Constant(u: String))) ⇒ l -> u
      case _ ⇒ c.abort(lowerTree.pos, "Character ranges must be specified with string literals")
    }
    if (lower.length != 1) c.abort(lowerTree.pos, "lower bound must be a single char string")
    if (upper.length != 1) c.abort(upperTree.pos, "upper bound must be a single char string")
    val lowerBoundChar = lower.charAt(0)
    val upperBoundChar = upper.charAt(0)
    if (lowerBoundChar > upperBoundChar) c.abort(lowerTree.pos, "lower bound must not be > upper bound")
    CharacterRange(lowerBoundChar, upperBoundChar)
  }

  case class CharacterRange(lowerBound: Char, upperBound: Char) extends TerminalOpTree {
    def ruleTraceTerminal = q"$prefix.RuleTrace.CharRange($lowerBound, $upperBound)"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"""
        val char = __psi.cursorChar
        $lowerBound <= char && char <= $upperBound && __psi.advance()"""
      if (wrapped) q"$unwrappedTree && __psi.updateMaxCursor() || __psi.registerMismatch()" else unwrappedTree
    }
  }

  case class Action(actionTree: Tree, actionTypeTree: Tree) extends DefaultNonTerminalOpTree {
    val actionType: List[Type] = actionTypeTree.tpe match {
      case TypeRef(_, _, args) if args.nonEmpty ⇒ args
      case x                                    ⇒ c.abort(actionTree.pos, "Unexpected action type: " + x)
    }
    def ruleTraceNonTerminalKey = reify(RuleTrace.Action).tree
    def renderInner(wrapped: Boolean): Tree = {
      val argTypes = actionType dropRight 1

      def popToVals(valNames: List[TermName]): List[Tree] =
        (valNames zip argTypes).map { case (n, t) ⇒ q"val $n = __psi.valueStack.pop().asInstanceOf[$t]" }.reverse

      def actionBody(tree: Tree): Tree =
        tree match {
          case Block(statements, res) ⇒ block(statements, actionBody(res))

          case x @ (Ident(_) | Select(_, _)) ⇒
            val valNames: List[TermName] = argTypes.indices.map { i ⇒ TermName("value" + i) }(collection.breakOut)
            val args = valNames map Ident.apply
            block(popToVals(valNames), q"__psi.push($x(..$args))")

          case q"(..$args ⇒ $body)" ⇒
            def rewrite(tree: Tree): Tree =
              tree match {
                case Block(statements, res)            ⇒ block(statements, rewrite(res))
                case x if actionType.last <:< RuleType ⇒ expand(x, wrapped)
                case x                                 ⇒ q"__psi.push($x)"
              }
            block(popToVals(args.map(_.name)), rewrite(body))
        }

      actionBody(actionTree)
    }
  }

  case class Fail(stringTree: Tree) extends OpTree {
    def render(wrapped: Boolean): Tree = q"throw new $prefix.ParserStateImpl.Fail($stringTree)"
  }

  case class Named(op: OpTree, stringTree: Tree) extends DefaultNonTerminalOpTree {
    def ruleTraceNonTerminalKey = q"$prefix.RuleTrace.Named($stringTree)"
    def renderInner(wrapped: Boolean): Tree = op.render(wrapped)
  }

  /////////////////////////////////// helpers ////////////////////////////////////

  class Collector(
    val valBuilder: Tree,
    val popToBuilder: Tree,
    val pushBuilderResult: Tree,
    val pushSomePop: Tree,
    val pushNone: Tree)

  lazy val rule0Collector = {
    val unit = q"()"
    new Collector(unit, unit, q"true", unit, unit)
  }

  lazy val rule1Collector = new Collector(
    valBuilder = q"val builder = new scala.collection.immutable.VectorBuilder[Any]",
    popToBuilder = q"builder += __psi.valueStack.pop()",
    pushBuilderResult = q"__psi.valueStack.push(builder.result()); true",
    pushSomePop = q"__psi.valueStack.push(Some(__psi.valueStack.pop()))",
    pushNone = q"__psi.valueStack.push(None)")

  type Separator = Boolean ⇒ Tree

  def Separator(op: OpTree): Separator = wrapped ⇒ op.render(wrapped)

  lazy val HListConsClassSymbol = c.mirror.staticClass("shapeless.$colon$colon")
  lazy val HNilClassSymbol = c.mirror.staticClass("shapeless.HNil")

  lazy val RuleType = typeOf[Rule[_, _, _]]
  lazy val Rule1XType = typeOf[Rule1X[_, _, _, _]]
  lazy val Rule2XType = typeOf[Rule2X[_, _, _, _, _]]
  lazy val Rule3XType = typeOf[Rule3X[_, _, _, _, _, _]]

  lazy val prefix: Tree = q"_root_.org.parboiled2"

  // tries to match and expand the leaves of the given Tree
  def expand(tree: Tree, wrapped: Boolean): Tree =
    tree match {
      case Block(statements, res)     ⇒ block(statements, expand(res, wrapped))
      case If(cond, thenExp, elseExp) ⇒ If(cond, expand(thenExp, wrapped), expand(elseExp, wrapped))
      case Match(selector, cases)     ⇒ Match(selector, cases.map(expand(_, wrapped).asInstanceOf[CaseDef]))
      case CaseDef(pat, guard, body)  ⇒ CaseDef(pat, guard, expand(body, wrapped))
      case x                          ⇒ opTreePF.andThen(_.render(wrapped)).applyOrElse(tree, identity[Tree])
    }

  private def callName(tree: Tree, errorMsgPrefix: String): Tree =
    callName(tree) match {
      case Some(name) ⇒ Literal(Constant(name))
      case None       ⇒ c.abort(tree.pos, errorMsgPrefix + tree)
    }

  @tailrec
  private def callName(tree: Tree): Option[String] =
    tree match {
      case Ident(name)       ⇒ Some(name.decodedName.toString)
      case Select(_, name)   ⇒ Some(name.decodedName.toString)
      case Apply(fun, _)     ⇒ callName(fun)
      case TypeApply(fun, _) ⇒ callName(fun)
      case _                 ⇒ None
    }

  def block(a: Tree, b: Tree): Tree =
    a match {
      case Block(a1, a2) ⇒ b match {
        case Block(b1, b2) ⇒ Block(a1 ::: a2 :: b1, b2)
        case _             ⇒ Block(a1 ::: a2 :: Nil, b)
      }
      case _ ⇒ b match {
        case Block(b1, b2) ⇒ Block(a :: b1, b2)
        case _             ⇒ Block(a :: Nil, b)
      }
    }

  def block(stmts: List[Tree], expr: Tree): Tree =
    expr match {
      case Block(a, b) ⇒ block(stmts ::: a ::: Nil, b)
      case _           ⇒ Block(stmts, expr)
    }

  object StateAccessTransformer extends Transformer with (Tree ⇒ Tree) {
    def apply(tree: Tree): Tree = c.untypecheck(transform(tree))
    override def transform(tree: Tree): Tree =
      tree match {
        case q"$a.this.state" ⇒ q"__psi"
        case q"$a.this.ctx"   ⇒ q"__psi.ctx"
        case _                ⇒ super.transform(tree)
      }
  }

  def isSubType(t: Tree, that: Type): Boolean = t.tpe != null && t.tpe <:< that

  sealed trait ResultExpression {
    def get: Tree
    def mapStatements(f: Tree ⇒ Tree): ResultExpression
    def mapResult(f: Tree ⇒ Tree): ResultExpression = ResultExpression(mapResultAndGet(f))
    def mapResultAndGet(f: Tree ⇒ Tree): Tree
  }
  object ResultExpression {
    final case class Single(result: Tree) extends ResultExpression {
      def get = result
      def mapStatements(f: Tree ⇒ Tree) = this
      def mapResultAndGet(f: Tree ⇒ Tree) = f(result)
    }
    final case class BlockExpr(stmts: List[Tree], result: Tree) extends ResultExpression {
      def get = Block(stmts, result)
      def mapStatements(f: Tree ⇒ Tree) = BlockExpr(stmts map f, result)
      def mapResultAndGet(f: Tree ⇒ Tree) = block(stmts, f(result))
    }
    def apply(tree: Tree): ResultExpression =
      tree match {
        case Block(stmts, res) ⇒ BlockExpr(stmts, res)
        case _                 ⇒ Single(tree)
      }
  }
}
