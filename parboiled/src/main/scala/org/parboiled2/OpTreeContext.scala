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

import shapeless._
import scala.annotation.tailrec

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  abstract class OpTree {
    def render(ruleName: String = ""): Expr[RuleX]
  }

  //  implicit class DummyStringContext(val sc: StringContext) extends AnyVal {
  //    def q(args: Any*): Tree = ???
  //  }

  object OpTree {
    def apply(tree: Tree): OpTree = {
      def isForRule1(t: Tree): Boolean =
        t match {
          case q"parboiled2.this.$a.forReduction[$b, $c]" ⇒ false
          case q"parboiled2.this.$a.forRule0" ⇒ false
          case q"parboiled2.this.$a.forRule1[$b]" ⇒ true
          case _ ⇒ c.abort(tree.pos, "Unexpected Optionalizer/Sequencer: " + t)
        }
      def sequencer(tree: Tree): Sequencer =
        if (isForRule1(tree)) new Sequencer {
          def valBuilder = c.Expr[Unit](q"val builder = new scala.collection.immutable.VectorBuilder[Any]")
          def popToBuilder = c.Expr[Unit](q"builder += p.__valueStack.pop()")
          def builderPushResult = c.Expr[Unit](q"p.__valueStack.push(builder.result())")
        }
        else new Sequencer {
          def valBuilder = c.literalUnit
          def popToBuilder = c.literalUnit
          def builderPushResult = c.literalUnit
        }

      tree match {
        case q"$lhs.~[$a, $b]($rhs)($c, $d)"         ⇒ Sequence(OpTree(lhs), OpTree(rhs))
        case q"$lhs.|[$a, $b]($rhs)"                 ⇒ FirstOf(OpTree(lhs), OpTree(rhs))
        case q"$a.this.str($s)"                      ⇒ LiteralString(s)
        case q"$a.this.ch($c)"                       ⇒ LiteralChar(c)
        case q"$a.this.ANY"                          ⇒ AnyChar
        case q"$a.this.EMPTY"                        ⇒ Empty
        case q"$a.this.optional[$b, $c]($arg)($o)"   ⇒ Optional(OpTree(arg), isForRule1(o))
        case q"$a.this.zeroOrMore[$b, $c]($arg)($s)" ⇒ ZeroOrMore(OpTree(arg), sequencer(s))
        case q"$a.this.oneOrMore[$b, $c]($arg)($s)"  ⇒ OneOrMore(OpTree(arg), sequencer(s))
        case q"$base.times[$ti, $to]($r)($s)"        ⇒ Times(base, OpTree(r), sequencer(s))
        case q"$a.this.&($arg)"                      ⇒ AndPredicate(OpTree(arg))
        case q"$a.unary_!()"                         ⇒ NotPredicate(OpTree(a))
        case q"$a.this.test($flag)"                  ⇒ SemanticPredicate(flag)
        case q"$a.this.capture[$b, $c]($arg)($d)"    ⇒ Capture(OpTree(arg))
        case q"$a.this.push[$b]($arg)($c)"           ⇒ PushAction(arg)
        case q"$a.this.$b"                           ⇒ RuleCall(tree)
        case q"$a.this.$b(..$c)"                     ⇒ RuleCall(tree)
        case q"$a.this.pimpString(${ Literal(Constant(l: String)) }).-(${ Literal(Constant(r: String)) })" ⇒
          CharacterRange(l, r, tree.pos)
        case q"$a.this.pimpActionOp[$b1, $b2]($r)($ops).~>.apply[..$e]($f)($g, parboiled2.this.Capture.capture[$ts])" ⇒
          Action(OpTree(r), f, ts.tpe.asInstanceOf[TypeRef].args)

        case _ ⇒ c.abort(tree.pos, "Invalid rule definition: " + tree)
      }
    }
  }

  trait Sequencer {
    def valBuilder: Expr[Unit]
    def popToBuilder: Expr[Unit]
    def builderPushResult: Expr[Unit]
  }

  // TODO: Having sequence be a simple (lhs, rhs) model causes us to allocate a mark on the stack
  // for every sequence concatenation. If we modeled sequences as a Seq[OpTree] we would be able to
  // reuse a single mutable mark for all intermediate markings in between elements. This will reduce
  // the stack size for all rules with sequences that are more than two elements long.
  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val left = lhs.render().splice
        if (left.matched) rhs.render().splice
        else left
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.Sequence(c.literal(ruleName).splice))
      }
    }
  }

  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val mark = p.__saveState
        val left = lhs.render().splice
        if (left.matched) left
        else {
          p.__restoreState(mark)
          rhs.render().splice
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.FirstOf(c.literal(ruleName).splice))
      }
    }
  }

  case class LiteralString(stringTree: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val string = c.Expr[String](stringTree).splice
      try {
        val p = c.prefix.splice
        @tailrec def rec(ix: Int): Boolean =
          if (ix < string.length)
            if (p.__currentChar == string.charAt(ix)) {
              p.__advance()
              rec(ix + 1)
            } else false
          else true
        if (rec(0)) Rule.Matched else {
          p.__registerCharMismatch()
          Rule.Mismatched
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.LiteralString(string, c.literal(ruleName).splice))
      }
    }
  }

  case class LiteralChar(charTree: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val char = c.Expr[Char](charTree).splice
      try {
        val p = c.prefix.splice
        if (p.__currentChar == char) {
          p.__advance()
          Rule.Matched
        } else {
          p.__registerCharMismatch()
          Rule.Mismatched
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.LiteralChar(char, c.literal(ruleName).splice))
      }
    }
  }

  case object AnyChar extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        if (p.__currentChar == EOI) {
          p.__registerCharMismatch()
          Rule.Mismatched
        } else {
          p.__advance()
          Rule.Matched
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.AnyChar(c.literal(ruleName).splice))
      }
    }
  }

  case object Empty extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      Rule.Matched
    }
  }

  case class Optional(op: OpTree, opIsRule1: Boolean) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val mark = p.__markCursor
        if (op.render().splice.matched) {
          c.Expr[Unit](if (opIsRule1) q"p.__valueStack.push(Some(p.__valueStack.pop())) " else q"()").splice
        } else {
          c.Expr[Unit](if (opIsRule1) q"p.__valueStack.push(None)" else q"()").splice
          p.__resetCursor(mark)
        }
        Rule.Matched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.Optional(c.literal(ruleName).splice))
      }
    }
  }

  case class ZeroOrMore(op: OpTree, sequencer: Sequencer) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        var mark = p.__saveState
        sequencer.valBuilder.splice
        while (op.render().splice.matched) {
          sequencer.popToBuilder.splice
          mark = p.__saveState
        }
        p.__restoreState(mark)
        sequencer.builderPushResult.splice
        Rule.Matched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.ZeroOrMore(c.literal(ruleName).splice))
      }
    }
  }

  case class OneOrMore(op: OpTree, sequencer: Sequencer) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val firstMark = p.__saveState
        var mark = firstMark
        sequencer.valBuilder.splice
        while (op.render().splice.matched) {
          sequencer.popToBuilder.splice
          mark = p.__saveState
        }
        if (mark != firstMark) {
          p.__restoreState(mark)
          sequencer.builderPushResult.splice
          Rule.Matched
        } else Rule.Mismatched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.OneOrMore(c.literal(ruleName).splice))
      }
    }
  }

  case class Times(min: Int, max: Int, op: OpTree, sequencer: Sequencer, separator: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        sequencer.valBuilder.splice

        @tailrec def rec(count: Int, mark: Parser.Mark): Boolean =
          if (op.render().splice.matched) {
            sequencer.popToBuilder.splice
            if (count == c.literal(max).splice) true
            else rec(count + 1, p.__saveState)
          } else (count > c.literal(min).splice) && { p.__restoreState(mark); true }

        if (rec(1, p.__saveState)) {
          sequencer.builderPushResult.splice
          Rule.Matched
        } else Rule.Mismatched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.Times(c.literal(min).splice, c.literal(max).splice, c.literal(ruleName).splice))
      }
    }
  }
  object Times {
    def apply(base: Tree, rule: OpTree, sequencer: Sequencer, separator: Tree = null): OpTree = {
      val sep = if (separator eq null) Empty else OpTree(separator)
      base match {
        case q"$a.this.int2range(${ Literal(Constant(i: Int)) })" ⇒
          if (i < 0) c.abort(base.pos, "`x` in `x.times` must be non-negative")
          else if (i == 1) rule
          else Times(i, i, rule, sequencer, sep)

        case q"$a.this.NTimes(scala.this.Predef.intWrapper(${ Literal(Constant(min: Int)) }).to(${ Literal(Constant(max: Int)) }))" ⇒
          if (min < 0) c.abort(base.pos, "`min` in `(min to max).times` must be non-negative")
          else if (max < 0) c.abort(base.pos, "`max` in `(min to max).times` must be non-negative")
          else if (max < min) c.abort(base.pos, "`max` in `(min to max).times` must be >= `min`")
          else Times(min, max, rule, sequencer, sep)

        case _ ⇒ c.abort(base.pos, "Invalid `x` in `x.times(...)`: " + base)
      }
    }
  }

  abstract class SyntacticPredicate extends OpTree {
    def op: OpTree
    def renderMatch(): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val mark = p.__saveState
      val result = op.render().splice
      p.__restoreState(mark)
      result
    }
  }

  case class AndPredicate(op: OpTree) extends SyntacticPredicate {
    def render(ruleName: String): Expr[RuleX] = reify {
      try renderMatch().splice
      catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.AndPredicate(c.literal(ruleName).splice))
      }
    }
  }

  case class NotPredicate(op: OpTree) extends SyntacticPredicate {
    def render(ruleName: String): Expr[RuleX] = reify {
      try if (renderMatch().splice.matched) Rule.Mismatched else Rule.Matched
      catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.NotPredicate(c.literal(ruleName).splice))
      }
    }
  }

  case class SemanticPredicate(flagTree: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        if (c.Expr[Boolean](flagTree).splice) Rule.Matched
        else {
          p.__registerCharMismatch()
          Rule.Mismatched
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒ e.save(RuleFrame.SemanticPredicate(c.literal(ruleName).splice))
      }
    }
  }

  case class Capture(op: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val mark = p.__markCursor
      val result = op.render().splice
      if (result.matched) p.__valueStack.push(p.__sliceInput(mark))
      result
    }
  }

  case class PushAction(arg: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = {
      def unrollArg(tree: Tree): List[Tree] = tree match {
        // 1 :: "a" :: HNil ⇒ 1 :: unrollArg("a" :: HNil)
        case Block(List(ValDef(_, _, _, q"$v")),
          q"shapeless.this.HList.hlistOps[${ _ }]($innerBlock).::[${ _ }](${ _ })") ⇒ v :: unrollArg(innerBlock)
        // 1 :: HNil ⇒ List(1)
        case Block(List(ValDef(_, _, _, q"$v")), q"shapeless.HNil.::[${ _ }](${ _ })") ⇒ List(v)
        // HNil
        case q"shapeless.HNil" ⇒ List()
        // Single element
        case q"$v" ⇒ List(v)
      }
      val stackPushes = unrollArg(arg) map { case v ⇒ q"p.__valueStack.push($v)" }

      // for some reason `reify` doesn't seem to work here
      c.Expr[RuleX](q"""
        val p = ${c.prefix}
        ..$stackPushes
        Rule.Matched
      """)
    }
  }

  case class RuleCall(methodCall: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try c.Expr[RuleX](methodCall).splice
      catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.RuleCall(c.literal(ruleName).splice, c.literal(show(methodCall)).splice))
      }
    }
  }

  case class CharacterRange(lowerBound: Char, upperBound: Char) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val char = p.__currentChar
        if (c.literal(lowerBound).splice <= char && char <= c.literal(upperBound).splice) {
          p.__advance()
          Rule.Matched
        } else {
          p.__registerCharMismatch()
          Rule.Mismatched
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.CharacterRange(c.literal(lowerBound).splice, c.literal(upperBound).splice, c.literal(ruleName).splice))
      }
    }
  }
  object CharacterRange {
    def apply(lower: String, upper: String, pos: Position): CharacterRange = {
      if (lower.length != 1) c.abort(pos, "lower bound must be a single char string")
      if (upper.length != 1) c.abort(pos, "upper bound must be a single char string")
      val lowerBoundChar = lower.charAt(0)
      val upperBoundChar = upper.charAt(0)
      if (lowerBoundChar > upperBoundChar) c.abort(pos, "lower bound must not be > upper bound")
      apply(lowerBoundChar, upperBoundChar)
    }
  }

  // NOTE: applicant might be:
  // - `Function(_, _)` in case of function application
  // - `Ident(_)` in case of case class application
  case class Action(op: OpTree, applicant: Tree, functionType: List[Type]) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = {
      val argTypes = functionType dropRight 1
      val argNames = argTypes.indices map { i ⇒ newTermName("value" + i) }

      def bodyIfMatched(tree: Tree): Tree = tree match {
        case Block(exprs, res) ⇒
          q"..$exprs; ${bodyIfMatched(res)}"
        case Ident(_) ⇒
          val functionParams = argNames map Ident.apply
          val valDefs = (argNames zip argTypes) map { case (n, t) ⇒ q"val $n = p.__valueStack.pop().asInstanceOf[$t]" }
          q"..${valDefs.reverse}; p.__valueStack.push($applicant(..$functionParams)); result"
        case q"( ..$args ⇒ $body )" ⇒
          val (exprs, res) = body match {
            case Block(exps, rs) ⇒ (exps, rs)
            case x               ⇒ (Nil, x)
          }

          // TODO: Reconsider type matching
          val bodyNew = functionType.last.toString match {
            case tp if tp.startsWith("org.parboiled2.Rule") ⇒ q"${OpTree(res).render()}"
            case tp if tp == "Unit" ⇒ q"$res; result"
            case _ ⇒ q"${PushAction(res).render()}"
          }
          val argsNew = args zip argTypes map { case (arg, t) ⇒ q"val ${arg.name} = p.__valueStack.pop().asInstanceOf[$t]" }
          q"..${argsNew.reverse}; ..$exprs; $bodyNew"
      }

      c.Expr[RuleX] {
        q"""
          val result = ${op.render()}
          if (result.matched) {
            val p = ${c.prefix}
            ${bodyIfMatched(c.resetAllAttrs(applicant))}
          }
          else result
        """
      }
    }
  }
}
