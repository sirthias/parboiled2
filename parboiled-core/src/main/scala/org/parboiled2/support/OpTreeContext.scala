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

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  sealed trait OpTree {
    def ruleFrame: Tree

    // renders a RuleX Tree
    def renderRule(ruleName: String): Tree = q"""
      // split out into separate method so as to not double the rule method size
      // which would effectively decrease method inlining by about 50%
      def wrapped: Boolean = {
        val p = ${c.prefix.tree}
        ${render(wrapped = true, ruleName)}
      }
      Rule {
        val p = ${c.prefix.tree}
        if (p.__collectingErrors) wrapped
        else ${render(wrapped = false)}
      }"""

    // renders a Boolean Tree
    def render(wrapped: Boolean, ruleName: String = ""): Tree =
      if (wrapped) q"""
        try ${renderInner(wrapped)}
        catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(RuleFrame($ruleFrame, ${c.literal(ruleName).tree}))
        }"""
      else renderInner(wrapped)

    // renders a Boolean Tree
    protected def renderInner(wrapped: Boolean): Tree
  }

  def collector(lifterTree: Tree): Collector =
    lifterTree match {
      case q"support.this.$a.forRule0[$b]" ⇒ rule0Collector
      case q"support.this.$a.forRule1[$b, $c]" ⇒ rule1Collector
      case q"support.this.$a.forReduction[$b, $c, $d]" ⇒ rule0Collector
      case x ⇒ c.abort(x.pos, "Unexpected Lifter: " + lifterTree)
    }

  val opTreePF: PartialFunction[Tree, OpTree] = {
    case q"$lhs.~[$a, $b]($rhs)($c, $d)"         ⇒ Sequence(OpTree(lhs), OpTree(rhs))
    case q"$lhs.|[$a, $b]($rhs)"                 ⇒ FirstOf(OpTree(lhs), OpTree(rhs))
    case q"$a.this.ch($c)"                       ⇒ CharMatch(c)
    case q"$a.this.str($s)"                      ⇒ StringMatch(s)
    case q"$a.this.ignoreCase($t)"               ⇒ IgnoreCase(t)
    case q"$a.this.predicate($p)"                ⇒ CharPredicateMatch(p)
    case q"$a.this.anyOf($s)"                    ⇒ AnyOf(s)
    case q"$a.this.ANY"                          ⇒ ANY
    case q"$a.this.optional[$b, $c]($arg)($o)"   ⇒ Optional(OpTree(arg), collector(o))
    case q"$a.this.zeroOrMore[$b, $c]($arg)($s)" ⇒ ZeroOrMore(OpTree(arg), collector(s))
    case q"$a.this.oneOrMore[$b, $c]($arg)($s)"  ⇒ OneOrMore(OpTree(arg), collector(s))
    case q"$base.times[$a, $b]($r)($s)"          ⇒ Times(base, OpTree(r), collector(s))
    case q"$a.this.&($arg)"                      ⇒ AndPredicate(OpTree(arg))
    case q"$a.unary_!()"                         ⇒ NotPredicate(OpTree(a))
    case q"$a.this.test($flag)"                  ⇒ SemanticPredicate(flag)
    case q"$a.this.capture[$b, $c]($arg)($d)"    ⇒ Capture(OpTree(arg))
    case q"$a.this.run[$b]($arg)($rr)"           ⇒ RunAction(arg, rr)
    case q"$a.this.push[$b]($arg)($hl)"          ⇒ PushAction(arg, hl)
    case q"$a.this.drop[$b]($hl)"                ⇒ DropAction(hl)
    case x @ q"$a.this.str2CharRangeSupport(${ Literal(Constant(l: String)) }).-(${ Literal(Constant(r: String)) })" ⇒
      CharRange(l, r, x.pos)
    case q"$a.this.rule2ActionOperator[$b1, $b2]($r)($o).~>.apply[..$e]($f)($g, support.this.FCapture.apply[$ts])" ⇒
      Sequence(OpTree(r), Action(f, ts))
    case x @ q"$a.this.rule2WithSeparatedBy[$b1, $b2]($base.$fun[$d, $e]($arg)($s)).separatedBy($sep)" ⇒
      val (op, coll, separator) = (OpTree(arg), collector(s), Separator(OpTree(sep)))
      fun.decoded match {
        case "zeroOrMore" ⇒ ZeroOrMore(op, coll, separator)
        case "oneOrMore"  ⇒ OneOrMore(op, coll, separator)
        case "times"      ⇒ Times(base, op, coll, separator)
        case _            ⇒ c.abort(x.pos, "Unexpected Repeated fun: " + fun)
      }
    case call @ (Apply(_, _) | Select(_, _) | Ident(_)) ⇒ RuleCall(call)
  }

  def OpTree(tree: Tree): OpTree =
    opTreePF.applyOrElse(tree, (t: Tree) ⇒ c.abort(t.pos, "Invalid rule definition: " + t))

  def Sequence(lhs: OpTree, rhs: OpTree): Sequence =
    lhs match {
      case Sequence(ops) ⇒ Sequence(ops :+ rhs)
      case _             ⇒ Sequence(Seq(lhs, rhs))
    }

  case class Sequence(ops: Seq[OpTree]) extends OpTree {
    require(ops.size >= 2)
    def ruleFrame = q"RuleFrame.Sequence(${c.literal(ops.size).tree})"
    def renderInner(wrapped: Boolean): Tree =
      ops.map(_.render(wrapped)).reduceLeft((l, r) ⇒ q"$l && $r")
  }

  def FirstOf(lhs: OpTree, rhs: OpTree): FirstOf =
    lhs match {
      case FirstOf(ops) ⇒ FirstOf(ops :+ rhs)
      case _            ⇒ FirstOf(Seq(lhs, rhs))
    }

  case class FirstOf(ops: Seq[OpTree]) extends OpTree {
    def ruleFrame = q"RuleFrame.FirstOf(${c.literal(ops.size).tree})"
    def renderInner(wrapped: Boolean): Tree =
      q"""val mark = p.__saveState; ${
        ops.map(_.render(wrapped)).reduceLeft((l, r) ⇒ q"$l || { p.__restoreState(mark); $r }")
      }"""
  }

  case class CharMatch(charTree: Tree) extends OpTree {
    def ruleFrame = q"RuleFrame.CharMatch($charTree)"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"p.cursorChar == $charTree && p.__advance()"
      if (wrapped) q"$unwrappedTree || p.__registerMismatch()" else unwrappedTree
    }
  }

  case class StringMatch(stringTree: Tree) extends OpTree {
    final private val autoExpandStringSize = 8

    def renderInner(wrapped: Boolean): Tree = `n/a`
    def ruleFrame = q"RuleFrame.StringMatch($stringTree)"
    override def render(wrapped: Boolean, ruleName: String = ""): Tree = {
      def renderStringLiteralMatch(str: String): Tree = {
        def unrollUnwrapped(ix: Int = 0): Tree =
          if (ix == str.length - 1) q"p.cursorChar == ${str.charAt(ix)} && p.__advance()"
          else q"p.cursorChar == ${str.charAt(ix)} && p.__advance() && ${unrollUnwrapped(ix + 1)}"

        def unrollWrapped(ix: Int = 0): Tree =
          if (ix == str.length) q"-1"
          else q"""
            if (p.cursorChar == ${str.charAt(ix)}) {
              p.__advance()
              ${unrollWrapped(ix + 1)}
            } else $ix
          """

        if (str.isEmpty) q"true"
        else if (wrapped) q"""
          val mismatchIx = ${unrollWrapped()}
          try mismatchIx == -1 || p.__registerMismatch()
          catch {
            case e: Parser.CollectingRuleStackException ⇒
              e.save(RuleFrame(RuleFrame.StringMatch($str), ${c.literal(ruleName).tree}),
                RuleFrame.CharMatch($str charAt mismatchIx))
          }"""
        else unrollUnwrapped()
      }

      def renderRegularStringMatch(): Tree =
        if (wrapped) q"""
          val string = $stringTree
          @annotation.tailrec def rec(ix: Int): Int =
            if (ix < string.length)
              if (p.cursorChar == string.charAt(ix)) {
                p.__advance()
                rec(ix + 1)
              } else ix
            else -1
          val mismatchIx = rec(0)
          try mismatchIx == -1 || p.__registerMismatch()
          catch {
            case e: Parser.CollectingRuleStackException ⇒
              e.save(RuleFrame(RuleFrame.StringMatch(string), ${c.literal(ruleName).tree}),
                RuleFrame.CharMatch(string charAt mismatchIx))
          }"""
        else q"""
          val string = $stringTree
          @annotation.tailrec def rec(ix: Int): Boolean =
            if (ix < string.length)
              if (p.cursorChar == string.charAt(ix)) {
                p.__advance()
                rec(ix + 1)
              } else false
            else true
          rec(0)"""

      stringTree match {
        case Literal(Constant(str: String)) if str.length <= autoExpandStringSize ⇒ renderStringLiteralMatch(str)
        case _ ⇒ renderRegularStringMatch()
      }
    }
  }

  def IgnoreCase(argTree: Tree): OpTree = {
    val argTypeSymbol = argTree.tpe.typeSymbol
    if (argTypeSymbol == definitions.CharClass) IgnoreCaseChar(argTree)
    else if (argTypeSymbol == definitions.StringClass) IgnoreCaseString(argTree)
    else c.abort(argTree.pos, "Unexpected `ignoreCase` argument type: " + argTypeSymbol)
  }

  case class IgnoreCaseChar(charTree: Tree) extends OpTree {
    def ruleFrame = q"RuleFrame.IgnoreCaseChar($charTree)"
    def renderInner(wrapped: Boolean): Tree =
      if (wrapped)
        q"if (p.cursorChar.toLower == $charTree) p.__advance() else p.__registerMismatch()"
      else
        q"p.cursorChar.toLower == $charTree && p.__advance()"
  }

  case class IgnoreCaseString(stringTree: Tree) extends OpTree {
    def renderInner(wrapped: Boolean): Tree = `n/a`
    def ruleFrame = q"RuleFrame.IgnoreCaseString($stringTree)"
    override def render(wrapped: Boolean, ruleName: String = ""): Tree =
      if (wrapped) q"""
        val string = $stringTree
        @annotation.tailrec def rec(ix: Int): Int =
          if (ix < string.length)
            if (p.cursorChar.toLower == string.charAt(ix)) {
              p.__advance()
              rec(ix + 1)
            } else ix
          else -1
        val mismatchIx = rec(0)
        try mismatchIx == -1 || p.__registerMismatch()
        catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(RuleFrame(RuleFrame.IgnoreCaseString(string), ${c.literal(ruleName).tree}),
              RuleFrame.CharMatch(string charAt mismatchIx))
        }"""
      else q"""
        val string = $stringTree
        @annotation.tailrec def rec(ix: Int): Boolean =
          if (ix < string.length)
            if (p.cursorChar.toLower == string.charAt(ix)) {
              p.__advance()
              rec(ix + 1)
            } else false
          else true
        rec(0)"""
  }

  case class CharPredicateMatch(predicateTree: Tree) extends OpTree {
    def predicateName = callName(predicateTree) getOrElse ""
    def ruleFrame = q"RuleFrame.CharPredicateMatch($predicateTree, ${c.literal(predicateName).tree})"
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"val pred = $predicateTree; pred(p.cursorChar) && p.__advance()"
      if (wrapped) q"$unwrappedTree || p.__registerMismatch()" else unwrappedTree
    }
  }

  case class AnyOf(stringTree: Tree) extends OpTree {
    def ruleFrame = q"RuleFrame.AnyOf($stringTree)"
    def renderInner(wrapped: Boolean): Tree = q"""
      val string = $stringTree
      val c = p.cursorChar
      @annotation.tailrec def rec(ix: Int): Boolean =
        if (ix < string.length)
          if (string.charAt(ix) == c) p.__advance()
          else rec(ix + 1)
        else false
      ${if (wrapped) q"rec(0) || p.__registerMismatch()" else q"rec(0)"}"""
  }

  case object ANY extends OpTree {
    def ruleFrame = reify(RuleFrame.ANY).tree
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"p.cursorChar != EOI && p.__advance()"
      if (wrapped) q"$unwrappedTree || p.__registerMismatch()" else unwrappedTree
    }
  }

  case class Optional(op: OpTree, collector: Collector) extends OpTree {
    def ruleFrame = reify(RuleFrame.Optional).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      val mark = p.__saveState
      if (${op.render(wrapped)}) {
        ${collector.pushSomePop}
      } else {
        p.__restoreState(mark)
        ${collector.pushNone}
      }
      true"""
  }

  case class ZeroOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends OpTree {
    def ruleFrame = reify(RuleFrame.ZeroOrMore).tree
    def renderInner(wrapped: Boolean): Tree = {
      val recurse =
        if (separator eq null) q"rec(p.__saveState)"
        else q"val m = p.__saveState; if (${separator(wrapped)}) rec(m) else m"

      q"""
      ${collector.valBuilder}

      @annotation.tailrec def rec(mark: Parser.Mark): Parser.Mark =
        if (${op.render(wrapped)}) {
          ${collector.popToBuilder}
          $recurse
        } else mark

      p.__restoreState(rec(p.__saveState))
      ${collector.pushBuilderResult}"""
    }
  }

  case class OneOrMore(op: OpTree, collector: Collector, separator: Separator = null) extends OpTree {
    def ruleFrame = reify(RuleFrame.OneOrMore).tree
    def renderInner(wrapped: Boolean): Tree = {
      val recurse =
        if (separator eq null) q"rec(p.__saveState)"
        else q"val m = p.__saveState; if (${separator(wrapped)}) rec(m) else m"

      q"""
      val firstMark = p.__saveState
      ${collector.valBuilder}

      @annotation.tailrec def rec(mark: Parser.Mark): Parser.Mark =
        if (${op.render(wrapped)}) {
          ${collector.popToBuilder}
          $recurse
        } else mark

      val mark = rec(firstMark)
      mark != firstMark && {
        p.__restoreState(mark)
        ${collector.pushBuilderResult}
      }"""
    }
  }

  def Times(base: Tree, rule: OpTree, collector: Collector, separator: Separator = null): OpTree =
    base match {
      case q"$a.this.int2NTimes($n)" ⇒ n match {
        case Literal(Constant(i: Int)) ⇒
          if (i < 0) c.abort(base.pos, "`x` in `x.times` must be non-negative")
          else if (i == 1) rule
          else Times(rule, q"val min, max = $n", collector, separator)
        case x @ (Ident(_) | Select(_, _)) ⇒ Times(rule, q"val min = $n; val max = min", collector, separator)
        case _                             ⇒ c.abort(n.pos, "Invalid int base expression for `.times(...)`: " + n)
      }
      case q"$a.this.range2NTimes($r)" ⇒ r match {
        case q"scala.this.Predef.intWrapper($mn).to($mx)" ⇒ (mn, mx) match {
          case (Literal(Constant(min: Int)), Literal(Constant(max: Int))) ⇒
            if (min < 0) c.abort(mn.pos, "`min` in `(min to max).times` must be non-negative")
            else if (max < 0) c.abort(mx.pos, "`max` in `(min to max).times` must be non-negative")
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

  case class Times(op: OpTree, init: Tree, collector: Collector, separator: Separator) extends OpTree {
    val Block(inits, _) = init
    def ruleFrame = q"..$inits; RuleFrame.Times(min, max)"
    def renderInner(wrapped: Boolean): Tree = {
      val recurse =
        if (separator eq null) q"rec(count + 1, p.__saveState)"
        else q"""
          val m = p.__saveState; if (${separator(wrapped)}) rec(count + 1, m)
          else (count >= min) && { p.__restoreState(m); true }"""

      q"""
      ${collector.valBuilder}
      ..$inits

      @annotation.tailrec def rec(count: Int, mark: Parser.Mark): Boolean = {
        if (${op.render(wrapped)}) {
          ${collector.popToBuilder}
          if (count < max) $recurse else true
        } else (count > min) && { p.__restoreState(mark); true }
      }

      (max <= 0) || rec(1, p.__saveState) && ${collector.pushBuilderResult}"""
    }
  }

  case class AndPredicate(op: OpTree) extends OpTree {
    def ruleFrame = reify(RuleFrame.AndPredicate).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      val mark = p.__saveState
      val result = ${op.render(wrapped)}
      p.__restoreState(mark)
      result"""
  }

  case class NotPredicate(op: OpTree) extends OpTree {
    def renderInner(wrapped: Boolean): Tree = `n/a`
    def ruleFrame = reify(RuleFrame.NotPredicate).tree
    override def render(wrapped: Boolean, ruleName: String = ""): Tree = {
      val unwrappedTree = q"""
        val mark = p.__saveState
        val saved = p.__enterNotPredicate
        val result = ${op.render(wrapped)}
        p.__exitNotPredicate(saved)
        p.__restoreState(mark)
        !result"""
      if (wrapped) q"""
        try $unwrappedTree || p.__registerMismatch()
        catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(RuleFrame($ruleFrame, ${c.literal(ruleName).tree}), ${op.ruleFrame})
        }"""
      else unwrappedTree
    }
  }

  case class SemanticPredicate(flagTree: Tree) extends OpTree {
    def ruleFrame = reify(RuleFrame.SemanticPredicate).tree
    def renderInner(wrapped: Boolean): Tree =
      if (wrapped) flagTree else q"$flagTree || p.__registerMismatch()"
  }

  case class Capture(op: OpTree) extends OpTree {
    def ruleFrame = reify(RuleFrame.Capture).tree
    def renderInner(wrapped: Boolean): Tree = q"""
      val start = p.cursor
      ${op.render(wrapped)} && {p.valueStack.push(p.input.sliceString(start, p.cursor)); true}"""
  }

  case class RunAction(argTree: Tree, rrTree: Tree) extends OpTree {
    def ruleFrame = reify(RuleFrame.Run).tree
    def renderInner(wrapped: Boolean): Tree = {
      def renderFunctionAction(resultTypeTree: Tree, argTypeTrees: Tree*): Tree = {
        def actionBody(tree: Tree): Tree =
          tree match {
            case Block(statements, res) ⇒ block(statements, actionBody(res))

            case q"(..$args ⇒ $body)" ⇒
              def rewrite(tree: Tree): Tree =
                tree match {
                  case Block(statements, res) ⇒ block(statements, rewrite(res))
                  case x if resultTypeTree.tpe <:< typeOf[Rule[_, _]] ⇒ matchAndExpandOpTreeIfPossible(x, wrapped)
                  case x ⇒ q"p.__push($x)"
                }
              val valDefs = args.zip(argTypeTrees).map { case (a, t) ⇒ q"val ${a.name} = p.valueStack.pop().asInstanceOf[${t.tpe}]" }.reverse
              block(valDefs, rewrite(body))

            case x ⇒ c.abort(argTree.pos, "Unexpected `run` argument: " + show(argTree))
          }

        actionBody(c.resetLocalAttrs(argTree))
      }

      rrTree match {
        case q"support.this.RunResult.forAny[$t]" ⇒ block(argTree, c.literalTrue.tree)

        case q"support.this.RunResult.forRule[$t]" ⇒
          def body(tree: Tree): Tree =
            tree match {
              case Block(statements, res) ⇒ block(statements, body(res))
              case x                      ⇒ matchAndExpandOpTreeIfPossible(x, wrapped)
            }
          body(argTree)

        case q"support.this.RunResult.forF1[$z, $r, $in, $out]($a)" ⇒ renderFunctionAction(r, z)
        case q"support.this.RunResult.forF2[$y, $z, $r, $in, $out]($a)" ⇒ renderFunctionAction(r, y, z)
        case q"support.this.RunResult.forF3[$x, $y, $z, $r, $in, $out]($a)" ⇒ renderFunctionAction(r, x, y, z)
        case q"support.this.RunResult.forF4[$w, $x, $y, $z, $r, $in, $out]($a)" ⇒ renderFunctionAction(r, w, x, y, z)
        case q"support.this.RunResult.forF5[$v, $w, $x, $y, $z, $r, $in, $out]($a)" ⇒ renderFunctionAction(r, v, w, x, y, z)

        case x ⇒ c.abort(rrTree.pos, "Unexpected RunResult: " + show(x))
      }
    }
  }

  case class PushAction(argTree: Tree, hlTree: Tree) extends OpTree {
    def ruleFrame = reify(RuleFrame.Push).tree
    def renderInner(wrapped: Boolean): Tree =
      block(hlTree match {
        case q"support.this.HListable.fromUnit"       ⇒ argTree
        case q"support.this.HListable.fromHList[$t]"  ⇒ q"p.valueStack.pushAll(${c.resetLocalAttrs(argTree)})"
        case q"support.this.HListable.fromAnyRef[$t]" ⇒ q"p.valueStack.push($argTree)"
        case x                                        ⇒ c.abort(hlTree.pos, "Unexpected HListable: " + show(x))
      }, c.literalTrue.tree)
  }

  case class DropAction(hlTree: Tree) extends OpTree {
    def ruleFrame = reify(RuleFrame.Drop).tree
    def renderInner(wrapped: Boolean): Tree =
      hlTree match {
        case q"support.this.HListable.fromUnit"       ⇒ c.literalTrue.tree
        case q"support.this.HListable.fromAnyRef[$t]" ⇒ q"p.valueStack.pop(); true"
        case q"support.this.HListable.fromHList[$t]" ⇒
          @tailrec def rec(t: Type, result: List[Tree] = Nil): List[Tree] =
            t match { // TODO: how can we use type quotes here, e.g. tq"shapeless.HNil"?
              case TypeRef(_, sym, List(_, tail)) if sym == HListConsTypeSymbol ⇒ rec(tail, q"p.valueStack.pop()" :: result)
              case TypeRef(_, sym, _) if sym == HNilTypeSymbol                  ⇒ result
            }
          Block(rec(t.tpe), c.literalTrue.tree)
        case x ⇒ c.abort(hlTree.pos, "Unexpected HListable: " + show(x))
      }
  }

  case class RuleCall(call: Tree) extends OpTree {
    def calleeName = callName(call) getOrElse c.abort(call.pos, "Illegal rule call: " + call)
    def ruleFrame = reify(RuleFrame.RuleCall(c.literal(calleeName).splice)).tree
    def renderInner(wrapped: Boolean): Tree = q"$call.matched"
  }

  def CharRange(lower: String, upper: String, pos: Position): CharacterRange = {
    if (lower.length != 1) c.abort(pos, "lower bound must be a single char string")
    if (upper.length != 1) c.abort(pos, "upper bound must be a single char string")
    val lowerBoundChar = lower.charAt(0)
    val upperBoundChar = upper.charAt(0)
    if (lowerBoundChar > upperBoundChar) c.abort(pos, "lower bound must not be > upper bound")
    CharacterRange(lowerBoundChar, upperBoundChar)
  }

  case class CharacterRange(lowerBound: Char, upperBound: Char) extends OpTree {
    def ruleFrame = reify(RuleFrame.CharRange(c.literal(lowerBound).splice, c.literal(upperBound).splice)).tree
    def renderInner(wrapped: Boolean): Tree = {
      val unwrappedTree = q"""
        val char = p.cursorChar
        ${c.literal(lowerBound).tree} <= char && char <= ${c.literal(upperBound).tree} && p.__advance()"""
      if (wrapped) q"$unwrappedTree || p.__registerMismatch()" else unwrappedTree
    }
  }

  case class Action(actionTree: Tree, actionTypeTree: Tree) extends OpTree {
    val actionType: List[Type] = actionTypeTree.tpe match {
      case TypeRef(_, _, args) if args.nonEmpty ⇒ args
      case x                                    ⇒ c.abort(actionTree.pos, "Unexpected action type: " + x)
    }
    def ruleFrame = reify(RuleFrame.Action).tree
    def renderInner(wrapped: Boolean): Tree = {
      val argTypes = actionType dropRight 1

      def popToVals(valNames: List[TermName]): List[Tree] =
        (valNames zip argTypes).map { case (n, t) ⇒ q"val $n = p.valueStack.pop().asInstanceOf[$t]" }.reverse

      def actionBody(tree: Tree): Tree =
        tree match {
          case Block(statements, res) ⇒ block(statements, actionBody(res))

          case x @ (Ident(_) | Select(_, _)) ⇒
            val valNames: List[TermName] = argTypes.indices.map { i ⇒ newTermName("value" + i) }(collection.breakOut)
            val args = valNames map Ident.apply
            block(popToVals(valNames), q"p.__push($x(..$args))")

          case q"(..$args ⇒ $body)" ⇒
            def rewrite(tree: Tree): Tree =
              tree match {
                case Block(statements, res) ⇒ block(statements, rewrite(res))
                case x if actionType.last <:< typeOf[Rule[_, _]] ⇒ matchAndExpandOpTreeIfPossible(x, wrapped)
                case x ⇒ q"p.__push($x)"
              }
            block(popToVals(args.map(_.name)), rewrite(body))
        }

      actionBody(c.resetLocalAttrs(actionTree))
    }
  }

  /////////////////////////////////// helpers ////////////////////////////////////

  class Collector(
    val valBuilder: Tree,
    val popToBuilder: Tree,
    val pushBuilderResult: Tree,
    val pushSomePop: Tree,
    val pushNone: Tree)

  lazy val rule0Collector = {
    val unit = c.literalUnit.tree
    new Collector(unit, unit, c.literalTrue.tree, unit, unit)
  }

  lazy val rule1Collector = new Collector(
    valBuilder = q"val builder = new org.parboiled2.support.SeqBuilder",
    popToBuilder = q"builder += p.valueStack.pop()",
    pushBuilderResult = q"p.valueStack.push(builder.result()); true",
    pushSomePop = q"p.valueStack.push(Some(p.valueStack.pop()))",
    pushNone = q"p.valueStack.push(None)")

  type Separator = Boolean ⇒ Tree

  def Separator(op: OpTree): Separator = wrapped ⇒ op.render(wrapped)

  lazy val HListConsTypeSymbol = typeOf[shapeless.::[_, _]].typeSymbol
  lazy val HNilTypeSymbol = typeOf[shapeless.HNil].typeSymbol

  def matchAndExpandOpTreeIfPossible(tree: Tree, wrapped: Boolean): Tree =
    opTreePF.andThen(_.render(wrapped)).applyOrElse(tree, (t: Tree) ⇒ q"$t.matched")

  @tailrec
  private def callName(tree: Tree): Option[String] =
    tree match {
      case Ident(name)     ⇒ Some(name.decoded)
      case Select(_, name) ⇒ Some(name.decoded)
      case Apply(fun, _)   ⇒ callName(fun)
      case _               ⇒ None
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
}
