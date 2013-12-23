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
import shapeless.HList
import org.parboiled2._

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  sealed trait OpTree {
    protected[this] abstract class RenderedBody {
      type T
      def init(ruleName: String): Expr[T]
      def process(ruleName: String): Expr[RuleX]
    }

    def renderedBody: RenderedBody

    def ruleFrame: Expr[RuleFrame.Anonymous]

    def render(ruleName: String = ""): Expr[RuleX] = {
      reify {
        renderedBody.init(ruleName).splice
        renderedBody.process(ruleName).splice
      }
    }
  }

  sealed trait OpTreeThrowsError extends OpTree {
    protected[this] abstract class RenderedBodyThrowsError extends RenderedBody {
      def ruleFrames(ruleName: String): Expr[List[RuleFrame]]
    }

    def renderedBody: RenderedBodyThrowsError

    override def render(ruleName: String): Expr[RuleX] = reify {
      val arg = renderedBody.init(ruleName).splice
      if (c.prefix.splice.__collectingErrors) {
        try renderedBody.process(ruleName).splice
        catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(renderedBody.ruleFrames(ruleName).splice: _*)
        }
      } else renderedBody.process(ruleName).splice
    }
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
    case q"$a.this.push[$b]($arg)($c)"           ⇒ PushAction(arg)
    case x @ q"$a.this.str2CharRangeSupport(${ Literal(Constant(l: String)) }).-(${ Literal(Constant(r: String)) })" ⇒
      CharRange(l, r, x.pos)
    case q"$a.this.rule2ActionOperator[$b1, $b2]($r)($o).~>.apply[..$e]($f)($g, support.this.FCapture.apply[$ts])" ⇒
      Action(OpTree(r), f, ts)
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

  case class Sequence(ops: Seq[OpTree]) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.Sequence(c.literal(ops.size).splice))

    def rec(ix: Int): Expr[RuleX] =
      if (ix < ops.size - 1) {
        val opName = newTermName("op" + ix)
        c.Expr[RuleX](q"""
          val $opName = ${ops(ix).render()}
          if ($opName.matched) ${rec(ix + 1)}
          else $opName""")
      } else ops(ix).render()

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"${rec(0)}")

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  def FirstOf(lhs: OpTree, rhs: OpTree): FirstOf =
    lhs match {
      case FirstOf(ops) ⇒ FirstOf(ops :+ rhs)
      case _            ⇒ FirstOf(Seq(lhs, rhs))
    }

  case class FirstOf(ops: Seq[OpTree]) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.FirstOf(c.literal(ops.size).splice))

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def rec(ix: Int): Expr[RuleX] =
        if (ix < ops.size - 1) {
          val opName = newTermName("op" + ix)
          c.Expr[RuleX](q"""
          val $opName = ${ops(ix).render()}
          if ($opName.matched) $opName
          else {
            p.__restoreState(mark)
            ${rec(ix + 1)}
          }""")
        } else ops(ix).render()

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
          val p = ${c.prefix}
          val mark = p.__saveState
          ${rec(0)}
      """)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class CharMatch(charTree: Tree) extends OpTreeThrowsError {
    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = reify {
        val char = c.Expr[Char](charTree).splice
      }

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
        val p = c.prefix.splice
        if (p.cursorChar == char) {
          p.__advance()
          Rule.Matched
        } else {
          p.__registerMismatch()
          Rule.Mismatched
        }
      """)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(RuleFrame.CharMatch(c.Expr[Char](q"char").splice), c.literal(ruleName).splice)))
    }

    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.CharMatch(c.Expr[Char](charTree).splice))
  }

  case class StringMatch(stringTree: Tree) extends OpTreeThrowsError {
    def renderedBody = new RenderedBodyThrowsError {
      type T = (Int, String)

      def init(ruleName: String): Expr[T] = reify {
        val string = c.Expr[String](stringTree).splice
        val p = c.prefix.splice
        @tailrec def rec(ix: Int): Int =
          if (ix < string.length)
            if (p.cursorChar == string.charAt(ix)) {
              p.__advance()
              rec(ix + 1)
            } else ix
          else -1
        val mismatchIx = rec(0)
        (mismatchIx, string)
      }

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
        val p = ${c.prefix}
        val (mismatchIx, string) = arg
        if (mismatchIx >= 0) {
          p.__registerMismatch()
          Rule.Mismatched
        } else Rule.Matched
      """)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame.StringMatch(c.Expr[String](stringTree).splice),
          c.Expr[RuleFrame](q"RuleFrame.CharMatch(arg._2 charAt arg._1)").splice))
    }

    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.StringMatch(c.Expr[String](stringTree).splice))
  }

  def IgnoreCase(argTree: Tree): OpTree = {
    val argTypeSymbol = argTree.tpe.typeSymbol
    if (argTypeSymbol == definitions.CharClass) IgnoreCaseChar(argTree)
    else if (argTypeSymbol == definitions.StringClass) IgnoreCaseString(argTree)
    else c.abort(argTree.pos, "Unexpected `ignoreCase` argument type: " + argTypeSymbol)
  }

  case class IgnoreCaseChar(charTree: Tree) extends OpTreeThrowsError {
    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = reify {
        val char = c.Expr[Char](charTree).splice
      }

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
        val p = c.prefix.splice
        if (p.cursorChar.toLower == char) {
          p.__advance()
          Rule.Matched
        } else {
          p.__registerMismatch()
          Rule.Mismatched
        }
      """)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(RuleFrame.IgnoreCaseChar(c.Expr[Char](q"char").splice), c.literal(ruleName).splice)))
    }

    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.CharMatch(c.Expr[Char](charTree).splice))
  }

  case class IgnoreCaseString(stringTree: Tree) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.IgnoreCaseString(c.Expr[String](stringTree).splice))

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = reify {
        val string = c.Expr[String](stringTree).splice
        val p = c.prefix.splice
        @tailrec def rec(ix: Int): Int =
          if (ix < string.length)
            if (p.cursorChar.toLower == string.charAt(ix)) {
              p.__advance()
              rec(ix + 1)
            } else ix
          else -1
        val mismatchIx = rec(0)
      }

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
        if (mismatchIx >= 0) {
          p.__registerMismatch()
          Rule.Mismatched
        } else Rule.Matched
      """)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice),
          c.Expr[RuleFrame](q"RuleFrame.CharMatch(string charAt mismatchIx)").splice))
    }
  }

  case class CharPredicateMatch(predicateTree: Tree) extends OpTreeThrowsError {
    def predicateName = callName(predicateTree) getOrElse ""

    def ruleFrame: Expr[RuleFrame.Anonymous] =
      reify(RuleFrame.CharPredicateMatch(c.Expr[CharPredicate](predicateTree).splice, c.literal(predicateName).splice))

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = reify {
        val predicate = c.Expr[CharPredicate](predicateTree).splice
      }

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
        val p = c.prefix.splice
        if (predicate(p.cursorChar)) {
          p.__advance()
          Rule.Matched
        } else {
          p.__registerMismatch()
          Rule.Mismatched
        }
      """)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class AnyOf(stringTree: Tree) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.AnyOf(c.Expr[String](stringTree).splice))

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = reify {
        val string = c.Expr[String](stringTree).splice
      }

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
        val p = c.prefix.splice
        val cursor = p.cursorChar
        @tailrec def rec(ix: Int): RuleX =
          if (ix < string.length) {
            if (cursor == string.charAt(ix)) {
              p.__advance()
              Rule.Matched
            } else rec(ix + 1)
          } else {
            p.__registerMismatch()
            Rule.Mismatched
          }
        rec(0)
      """)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case object ANY extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.ANY)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        if (p.cursorChar == EOI) {
          p.__registerMismatch()
          Rule.Mismatched
        } else {
          p.__advance()
          Rule.Matched
        }
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class Optional(op: OpTree, collector: Collector) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.Optional)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        val mark = p.__saveState
        if (op.render().splice.matched) {
          collector.pushSomePop.splice
        } else {
          p.__restoreState(mark)
          collector.pushNone.splice
        }
        Rule.Matched
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class ZeroOrMore(op: OpTree, collector: Collector, separator: Separator = emptySeparator) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.ZeroOrMore)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        collector.valBuilder.splice

        @tailrec def rec(mark: Parser.Mark): Parser.Mark =
          if (op.render().splice.matched) {
            collector.popToBuilder.splice
            val preSeparatorMark = p.__saveState
            val sepMatched = separator.tryMatch.splice
            if (sepMatched) rec(preSeparatorMark) else preSeparatorMark
          } else mark

        p.__restoreState(rec(p.__saveState))
        collector.builderPushResult.splice
        Rule.Matched
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class OneOrMore(op: OpTree, collector: Collector, separator: Separator = emptySeparator) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.OneOrMore)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        val firstMark = p.__saveState
        collector.valBuilder.splice

        @tailrec def rec(mark: Parser.Mark): Parser.Mark =
          if (op.render().splice.matched) {
            collector.popToBuilder.splice
            val preSeparatorMark = p.__saveState
            val sepMatched = separator.tryMatch.splice
            if (sepMatched) rec(preSeparatorMark) else preSeparatorMark
          } else mark

        val mark = rec(firstMark)
        if (mark != firstMark) {
          p.__restoreState(mark)
          collector.builderPushResult.splice
          Rule.Matched
        } else Rule.Mismatched
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(RuleFrame.OneOrMore, c.literal(ruleName).splice)))
    }
  }

  def Times(base: Tree, rule: OpTree, collector: Collector, separator: Separator = emptySeparator): OpTree =
    base match {
      case q"$a.this.int2NTimes(${ Literal(Constant(i: Int)) })" ⇒
        if (i < 0) c.abort(base.pos, "`x` in `x.times` must be non-negative")
        else if (i == 1) rule
        else Times(i, i, rule, collector, separator)

      case q"$a.this.range2NTimes(scala.this.Predef.intWrapper(${ Literal(Constant(min: Int)) }).to(${ Literal(Constant(max: Int)) }))" ⇒
        if (min < 0) c.abort(base.pos, "`min` in `(min to max).times` must be non-negative")
        else if (max < 0) c.abort(base.pos, "`max` in `(min to max).times` must be non-negative")
        else if (max < min) c.abort(base.pos, "`max` in `(min to max).times` must be >= `min`")
        else Times(min, max, rule, collector, separator)

      case _ ⇒ c.abort(base.pos, "Invalid `x` in `x.times(...)`: " + base)
    }

  case class Times(min: Int, max: Int, op: OpTree, collector: Collector, separator: Separator) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.Times(c.literal(min).splice, c.literal(max).splice))

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        collector.valBuilder.splice

        @tailrec def rec(count: Int, mark: Parser.Mark): Boolean = {
          if (op.render().splice.matched) {
            collector.popToBuilder.splice
            if (count < c.literal(max).splice) {
              val preSeparatorMark = p.__saveState
              val sepMatched = separator.tryMatch.splice
              if (sepMatched) rec(count + 1, preSeparatorMark)
              else (count >= c.literal(min).splice) && { p.__restoreState(preSeparatorMark); true }
            } else true
          } else (count > c.literal(min).splice) && { p.__restoreState(mark); true }
        }

        if (rec(1, p.__saveState)) {
          collector.builderPushResult.splice
          Rule.Matched
        } else Rule.Mismatched
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class AndPredicate(op: OpTree) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.AndPredicate)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        val mark = p.__saveState
        val result = op.render().splice
        p.__restoreState(mark)
        result
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(RuleFrame.AndPredicate, c.literal(ruleName).splice)))
    }
  }

  case class NotPredicate(op: OpTree) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.NotPredicate)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        val mark = p.__saveState
        val saved = p.__enterNotPredicate
        val result = op.render().splice
        p.__exitNotPredicate(saved)
        p.__restoreState(mark)
        if (result.matched) {
          p.__registerMismatch()
          Rule.Mismatched
        } else Rule.Matched
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(RuleFrame.NotPredicate, c.literal(ruleName).splice), op.ruleFrame.splice))
    }
  }

  case class SemanticPredicate(flagTree: Tree) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.SemanticPredicate)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        if (c.Expr[Boolean](flagTree).splice) Rule.Matched
        else {
          p.__registerMismatch()
          Rule.Mismatched
        }
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(RuleFrame.SemanticPredicate, c.literal(ruleName).splice)))
    }
  }

  case class Capture(op: OpTree) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.Capture)

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        val start = p.cursor
        val result = op.render().splice
        if (result.matched) p.valueStack.push(p.input.sliceString(start, p.cursor))
        result
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class RunAction(argTree: Tree, rrTree: Tree) extends OpTree {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.Run)

    def renderedBody = new RenderedBody {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = {
        rrTree match {
          case q"support.this.RunResult.forAny[$t]" ⇒
            reify {
              c.Expr[Unit](argTree).splice
              Rule.Matched
            }

          case q"support.this.RunResult.forRule[$t]" ⇒
            def body(tree: Tree): Tree =
              tree match {
                case Block(statements, res) ⇒ Block(statements, body(res))
                case x                      ⇒ matchAndExpandOpTreeIfPossible(x)
              }
            c.Expr[RuleX](body(argTree))

          case _ ⇒
            c.abort(rrTree.pos, "Unexpected RunResult expression: " + show(rrTree))
        }
      }
    }
  }

  case class PushAction(arg: Tree) extends OpTree {
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.Push)

    def renderedBody = new RenderedBody {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        val value: Any = c.Expr[Any](arg).splice
        value match {
          case ()       ⇒
          case x: HList ⇒ p.valueStack.pushAll(x)
          case x        ⇒ p.valueStack.push(x)
        }
        Rule.Matched
      }
    }
  }

  case class RuleCall(call: Tree) extends OpTreeThrowsError {
    def calleeName = callName(call) getOrElse c.abort(call.pos, "Illegal rule call: " + call)
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.RuleCall(c.literal(calleeName).splice))

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = c.Expr[RuleX](call)

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  def CharRange(lower: String, upper: String, pos: Position): CharacterRange = {
    if (lower.length != 1) c.abort(pos, "lower bound must be a single char string")
    if (upper.length != 1) c.abort(pos, "upper bound must be a single char string")
    val lowerBoundChar = lower.charAt(0)
    val upperBoundChar = upper.charAt(0)
    if (lowerBoundChar > upperBoundChar) c.abort(pos, "lower bound must not be > upper bound")
    CharacterRange(lowerBoundChar, upperBoundChar)
  }

  case class CharacterRange(lowerBound: Char, upperBound: Char) extends OpTreeThrowsError {
    def ruleFrame: Expr[RuleFrame.Anonymous] =
      reify(RuleFrame.CharRange(c.literal(lowerBound).splice, c.literal(upperBound).splice))

    def renderedBody = new RenderedBodyThrowsError {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = reify {
        val p = c.prefix.splice
        val char = p.cursorChar
        if (c.literal(lowerBound).splice <= char && char <= c.literal(upperBound).splice) {
          p.__advance()
          Rule.Matched
        } else {
          p.__registerMismatch()
          Rule.Mismatched
        }
      }

      def ruleFrames(ruleName: String): Expr[List[RuleFrame]] =
        reify(List(RuleFrame(ruleFrame.splice, c.literal(ruleName).splice)))
    }
  }

  case class Action(op: OpTree, actionTree: Tree, actionTypeTree: Tree) extends OpTree {
    val actionType: List[Type] = actionTypeTree.tpe match {
      case TypeRef(_, _, args) if args.nonEmpty ⇒ args
      case x                                    ⇒ c.abort(actionTree.pos, "Unexpected action type: " + x)
    }
    def ruleFrame: Expr[RuleFrame.Anonymous] = reify(RuleFrame.Action)

    def renderedBody = new RenderedBody {
      type T = Unit

      def init(ruleName: String): Expr[T] = c.literalUnit

      def process(ruleName: String): Expr[RuleX] = {
        val argTypes = actionType dropRight 1

        def popToVals(valNames: List[TermName]): List[Tree] =
          (valNames zip argTypes).map { case (n, t) ⇒ q"val $n = p.valueStack.pop().asInstanceOf[$t]" }.reverse

        def actionBody(tree: Tree): Tree =
          tree match {
            case Block(statements, res) ⇒ Block(statements, actionBody(res))

            case x @ (Ident(_) | Select(_, _)) ⇒
              val valNames: List[TermName] = argTypes.indices.map { i ⇒ newTermName("value" + i) }(collection.breakOut)
              val args = valNames map Ident.apply
              Block(popToVals(valNames), PushAction(q"$x(..$args)").render().tree)

            case q"(..$args ⇒ $body)" ⇒
              val (expressions, res) = body match {
                case Block(exps, rs) ⇒ (exps, rs)
                case x               ⇒ (Nil, x)
              }
              val resTree =
                if (actionType.last.typeSymbol == ruleTypeSymbol) matchAndExpandOpTreeIfPossible(res)
                else PushAction(res).render().tree
              Block(popToVals(args.map(_.name)) ::: expressions, resTree)
          }

        reify {
          val result = op.render().splice
          if (result.matched) {
            val p = c.prefix.splice
            c.Expr[RuleX](actionBody(c.resetAllAttrs(actionTree))).splice
          } else result
        }
      }
    }
  }

  /////////////////////////////////// helpers ////////////////////////////////////

  class Collector(
    val valBuilder: Expr[Unit],
    val popToBuilder: Expr[Unit],
    val builderPushResult: Expr[Unit],
    val pushSomePop: Expr[Unit],
    val pushNone: Expr[Unit])

  lazy val rule0Collector = new Collector(c.literalUnit, c.literalUnit, c.literalUnit, c.literalUnit, c.literalUnit)

  lazy val rule1Collector = new Collector(
    valBuilder = c.Expr[Unit](q"val builder = new scala.collection.immutable.VectorBuilder[Any]"),
    popToBuilder = c.Expr[Unit](q"builder += p.valueStack.pop()"),
    builderPushResult = c.Expr[Unit](q"p.valueStack.push(builder.result())"),
    pushSomePop = c.Expr[Unit](q"p.valueStack.push(Some(p.valueStack.pop()))"),
    pushNone = c.Expr[Unit](q"p.valueStack.push(None)"))

  class Separator(val tryMatch: Expr[Boolean])

  lazy val emptySeparator = new Separator(c.literalTrue)

  def Separator(op: OpTree) = new Separator(reify(op.render().splice.matched))

  lazy val ruleTypeSymbol = c.mirror.staticClass("org.parboiled2.Rule")

  def matchAndExpandOpTreeIfPossible(tree: Tree): Tree =
    opTreePF.andThen(_.render().tree).applyOrElse(tree, identity(_: Tree))

  @tailrec
  private def callName(tree: Tree): Option[String] =
    tree match {
      case Ident(name)     ⇒ Some(name.decoded)
      case Select(_, name) ⇒ Some(name.decoded)
      case Apply(fun, _)   ⇒ callName(fun)
      case _               ⇒ None
    }
}
