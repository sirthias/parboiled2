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

import scala.collection.immutable.VectorBuilder
import shapeless._

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  type RuleX = Rule[_ <: HList, _ <: HList]

  abstract class OpTree {
    def render(ruleName: String = ""): Expr[RuleX]
  }

  object OpTree {
    // TODO: DRY-up once we have the first complete implementation of all DSL elements
    def apply(tree: Tree): OpTree =
      tree match {
        case Apply(Apply(TypeApply(Select(lhs, Decoded("~")), _), List(rhs)), _) ⇒ Sequence(OpTree(lhs), OpTree(rhs))

        case Apply(TypeApply(Select(lhs, Decoded("|")), _), List(rhs)) ⇒ FirstOf(OpTree(lhs), OpTree(rhs))

        case Apply(Select(This(_), Decoded("str")), List(Literal(Constant(s: String)))) ⇒ LiteralString(s)

        case Apply(Select(This(_), Decoded("ch")), List(Select(This(_), Decoded("EOI")))) ⇒ LiteralChar(EOI)

        case Apply(Select(This(_), Decoded("ch")), List(Literal(Constant(c: Char)))) ⇒ LiteralChar(c)

        case Apply(Apply(TypeApply(Select(This(_), Decoded("optional")), _), List(arg)), _) ⇒ Optional(OpTree(arg))

        case Apply(Apply(TypeApply(Select(This(_), Decoded("zeroOrMore")), _), List(arg)), _) ⇒ ZeroOrMore(OpTree(arg))

        case Apply(Apply(TypeApply(Select(This(_), Decoded("oneOrMore")), _), List(arg)), _) ⇒ OneOrMore(OpTree(arg))

        case Apply(Apply(TypeApply(Select(This(_), Decoded("capture")), _), List(arg)), _) ⇒ Capture(OpTree(arg))

        case Apply(Select(This(_), Decoded("&")), List(arg)) ⇒ AndPredicate(OpTree(arg))

        case x @ Select(This(_), _) ⇒ RuleCall(x)
        case x @ Apply(Select(This(_), _), _) ⇒ RuleCall(x)

        case Apply(Select(arg, Decoded("unary_!")), List()) ⇒ NotPredicate(OpTree(arg))

        case Apply(Apply(TypeApply(Select(Select(Apply(Apply(TypeApply(Select(This(_), Decoded("pimpActionOp")), _),
          List(r)), _), Decoded("~>")), Decoded("apply")), _), List(f)), _) ⇒ Action(OpTree(r), f)

        case Apply(Apply(TypeApply(Select(This(_), Decoded("push")), _), List(arg)), _) ⇒ PushAction(arg)

        case Apply(Select(Apply(Select(This(_), Decoded("pimpString")), List(Literal(Constant(l: String)))),
          Decoded("-")), List(Literal(Constant(r: String)))) ⇒ CharacterClass(l, r, tree.pos)

        case _ ⇒ c.abort(tree.pos, s"Invalid rule definition: $tree\n${showRaw(tree)}")
      }
  }

  // TODO: Having sequence be a simple (lhs, rhs) model causes us to allocate a mark on the stack
  // for every sequence concatenation. If we modeled sequences as a Seq[OpTree] we would be able to
  // reuse a single mutable mark for all intermediate markings in between elements. This will reduce
  // the stack size for all rules with sequences that are more than two elements long.
  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try Rule(lhs.render().splice.matched && rhs.render().splice.matched)
      catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.Sequence(c.literal(ruleName).splice))
      }
    }
  }

  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val mark = p.mark
        val left = lhs.render().splice
        if (left.matched) left
        else {
          p.reset(mark)
          rhs.render().splice
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.FirstOf(c.literal(ruleName).splice))
      }
    }
  }

  case class LiteralString(s: String) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val string = c.literal(s).splice
      try {
        val p = c.prefix.splice
        var ix = 0
        while (ix < string.length && p.nextChar() == string.charAt(ix)) ix += 1
        Rule(ix == string.length || p.onCharMismatch())
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.LiteralString(string, c.literal(ruleName).splice))
      }
    }
  }

  case class LiteralChar(ch: Char) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val char = c.literal(ch).splice
      try {
        val p = c.prefix.splice
        Rule(p.nextChar() == char || p.onCharMismatch())
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.LiteralChar(char, c.literal(ruleName).splice))
      }
    }
  }

  case class Optional(op: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val mark = p.mark
        val savedTop = p.valueStack.top
        if (op.render().splice.matched) {
          // Optimization TODO: remove this per-invocation comparison (as its result is statically known)
          p.valueStack.top - savedTop match {
            case 0 ⇒
            case 1 ⇒ p.valueStack.push(Some(p.valueStack.pop()))
            case _ ⇒ p.valueStack.push(Some(p.valueStack.popHList(savedTop)))
          }
        } else p.reset(mark)
        Rule.matched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.Optional(c.literal(ruleName).splice))
      }
    }
  }

  case class ZeroOrMore(op: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val savedTop = p.valueStack.top
        var mark = p.mark
        var builder: VectorBuilder[Any] = null
        while (op.render().splice.matched) {
          // Optimization TODO: remove this per-invocation comparison (as its result is statically known)
          val valueCount = p.valueStack.top - savedTop
          if (valueCount > 0) {
            if (builder eq null) builder = new VectorBuilder[Any]
            builder += (if (valueCount == 1) p.valueStack.pop() else p.valueStack.popHList(savedTop))
          }
          mark = p.mark
        }
        p.reset(mark)
        if (builder ne null) p.valueStack.push(builder.result())
        Rule.matched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.ZeroOrMore(c.literal(ruleName).splice))
      }
    }
  }

  case class OneOrMore(op: OpTree) extends OpTree {
    // TODO: DRY up with `ZeroOrMore#render` implementation
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val savedTop = p.valueStack.top
        val firstMark = p.mark
        var mark = firstMark
        var builder: VectorBuilder[Any] = null
        while (op.render().splice.matched) {
          // Optimization TODO: remove this per-invocation comparison (as its result is statically known)
          val valueCount = p.valueStack.top - savedTop
          if (valueCount > 0) {
            if (builder eq null) builder = new VectorBuilder[Any]
            builder += (if (valueCount == 1) p.valueStack.pop() else p.valueStack.popHList(savedTop))
          }
          mark = p.mark
        }
        p.reset(mark)
        if (mark != firstMark) {
          if (builder ne null) p.valueStack.push(builder.result())
          Rule.matched
        } else Rule.mismatched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.OneOrMore(c.literal(ruleName).splice))
      }
    }
  }

  case class Capture(op: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val mark = p.inputStartMark
      val result = op.render().splice
      if (result.matched)
        p.valueStack.push(p.sliceInput(mark))
      result
    }
  }

  case class Action(op: OpTree, f: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      op.render().splice
    }
  }

  case class PushAction(arg: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      //val p = c.prefix.splice
      //val value = c.Expr[Any](arg).splice
      //p.valueStack.push(value)
      Rule.matched
    }
  }

  abstract class Predicate extends OpTree {
    def op: OpTree
    def renderMatch(): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val result = op.render().splice
      p.reset(mark)
      result
    }
  }

  case class AndPredicate(op: OpTree) extends Predicate {
    def render(ruleName: String): Expr[RuleX] = reify {
      try renderMatch().splice
      catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.AndPredicate(c.literal(ruleName).splice))
      }
    }
  }

  case class NotPredicate(op: OpTree) extends Predicate {
    def render(ruleName: String): Expr[RuleX] = reify {
      try Rule(!renderMatch().splice.matched)
      catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.NotPredicate(c.literal(ruleName).splice))
      }
    }
  }

  case class RuleCall(methodCall: Tree) extends OpTree {
    val calleeName = {
      val Select(This(tpName), termName) = methodCall
      s"$tpName.$termName"
    }
    def render(ruleName: String): Expr[RuleX] = reify {
      try c.Expr[RuleX](methodCall).splice
      catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.RuleCall(c.literal(ruleName).splice, c.literal(calleeName).splice))
      }
    }
  }

  case class CharacterClass(lowerBound: Char, upperBound: Char) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val char = p.nextChar()
        val ok = c.literal(lowerBound).splice <= char && char <= c.literal(upperBound).splice
        Rule(ok || p.onCharMismatch())
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.CharacterClass(c.literal(lowerBound).splice, c.literal(upperBound).splice, c.literal(ruleName).splice))
      }
    }
  }
  object CharacterClass {
    def apply(lower: String, upper: String, pos: Position): CharacterClass = {
      if (lower.length != 1) c.abort(pos, "lower bound must be a single char string")
      if (lower.length != 1) c.abort(pos, "upper bound must be a single char string")
      val lowerBoundChar = lower.charAt(0)
      val upperBoundChar = upper.charAt(0)
      if (lowerBoundChar > upperBoundChar) c.abort(pos, "lower bound must not be > upper bound")
      apply(lowerBoundChar, upperBoundChar)
    }
  }

  //  case class AnyCharacter() extends OpTree

  //  case class Grouping(n: OpTree) extends OpTree

  case object Empty extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      Rule.matched
    }
  }

  ////////////////// helpers ///////////////////

  private object Decoded {
    def unapply(name: Name): Option[String] = Some(name.decoded)
  }
}
