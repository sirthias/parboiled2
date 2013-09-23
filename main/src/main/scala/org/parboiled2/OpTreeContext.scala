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
    def apply(tree: Tree): OpTree = {
      def isForRule1(t: Tree): Boolean =
        t match {
          case q"parboiled2.this.$a.forReduction[$b, $c]" ⇒ false
          case q"parboiled2.this.$a.forRule0" ⇒ false
          case q"parboiled2.this.$a.forRule1[$b]" ⇒ true
          case _ ⇒ c.abort(tree.pos, "Unexpected Optionalizer/Sequencer: " + show(t))
        }

      tree match {
        case q"$lhs.~[$a, $b]($rhs)($c, $d)"                  ⇒ Sequence(OpTree(lhs), OpTree(rhs))
        case q"$lhs.|[$a, $b]($rhs)"                          ⇒ FirstOf(OpTree(lhs), OpTree(rhs))
        case q"$a.this.str($s)"                               ⇒ LiteralString(s)
        case q"$a.this.ch($c)"                                ⇒ LiteralChar(c)
        case q"$a.this.optional[$b, $c]($arg)($optionalizer)" ⇒ Optional(OpTree(arg), isForRule1(optionalizer))
        case q"$a.this.zeroOrMore[$b, $c]($arg)($sequencer)"  ⇒ ZeroOrMore(OpTree(arg), isForRule1(sequencer))
        case q"$a.this.oneOrMore[$b, $c]($arg)($sequencer)"   ⇒ OneOrMore(OpTree(arg), isForRule1(sequencer))
        case q"$a.this.capture[$b, $c]($arg)($d)"             ⇒ Capture(OpTree(arg))
        case q"$a.this.&($arg)"                               ⇒ AndPredicate(OpTree(arg))
        case q"$a.this.ANY"                                   ⇒ AnyChar
        case q"$a.this.$b"                                    ⇒ RuleCall(tree)
        case q"$a.this.$b(..$c)"                              ⇒ RuleCall(tree)
        case q"$a.unary_!()"                                  ⇒ NotPredicate(OpTree(a))
        case q"$a.this.pimpActionOp[$b1, $b2]($r)($ops).~>.apply[..$e]($f)($g, parboiled2.this.Arguments.$arity[..$ts])" ⇒
          Action(OpTree(r), f, ts.map(_.tpe))
        case q"$a.this.push[$b]($arg)($c)" ⇒ PushAction(arg)
        case q"$a.this.pimpString(${ Literal(Constant(l: String)) }).-(${ Literal(Constant(r: String)) })" ⇒
          CharacterClass(l, r, tree.pos)

        case _ ⇒ c.abort(tree.pos, s"Invalid rule definition: $tree\n${showRaw(tree)}")
      }
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
        val mark = p.markCursorAndValueStack
        val left = lhs.render().splice
        if (left.matched) left
        else {
          p.resetCursorAndValueStack(mark)
          rhs.render().splice
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.FirstOf(c.literal(ruleName).splice))
      }
    }
  }

  case class LiteralString(stringTree: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val string = c.Expr[String](stringTree).splice
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

  // NOTE: charTree:
  // - EOI
  // - literal Char
  case class LiteralChar(charTree: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val char = c.Expr[Char](charTree).splice
      try {
        val p = c.prefix.splice
        Rule(p.nextChar() == char || p.onCharMismatch())
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.LiteralChar(char, c.literal(ruleName).splice))
      }
    }
  }

  case object AnyChar extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        Rule(p.nextChar() != EOI || p.onCharMismatch())
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.AnyChar(c.literal(ruleName).splice))
      }
    }
  }

  case class Optional(op: OpTree, opIsRule1: Boolean) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = {
      reify {
        try {
          val p = c.prefix.splice
          val mark = p.markCursor
          if (op.render().splice.matched) {
            c.Expr[Unit](if (opIsRule1) q"p.valueStack.push(Some(p.valueStack.pop())) " else q"()").splice
          } else {
            c.Expr[Unit](if (opIsRule1) q"p.valueStack.push(None)" else q"()").splice
            p.resetCursor(mark)
          }
          Rule.matched
        } catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(RuleFrame.Optional(c.literal(ruleName).splice))
        }
      }
    }
  }

  case class ZeroOrMore(op: OpTree, opIsRule1: Boolean) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = {
      val block =
        if (opIsRule1)
          reify {
            val p = c.prefix.splice
            var mark = p.markCursorAndValueStack
            val builder = new VectorBuilder[Any]
            while (op.render().splice.matched) {
              builder += p.valueStack.pop()
              mark = p.markCursorAndValueStack
            }
            p.resetCursorAndValueStack(mark)
            p.valueStack.push(builder.result())
          }
        else
          reify {
            val p = c.prefix.splice
            var mark = p.markCursorAndValueStack
            while (op.render().splice.matched)
              mark = p.markCursorAndValueStack
            p.resetCursorAndValueStack(mark)
          }
      reify {
        try {
          block.splice
          Rule.matched
        } catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(RuleFrame.ZeroOrMore(c.literal(ruleName).splice))
        }
      }
    }
  }

  case class OneOrMore(op: OpTree, opIsRule1: Boolean) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = {
      val block =
        if (opIsRule1)
          reify {
            val p = c.prefix.splice
            val firstMark = p.markCursorAndValueStack
            var mark = firstMark
            val builder = new VectorBuilder[Any]
            while (op.render().splice.matched) {
              builder += p.valueStack.pop()
              mark = p.markCursorAndValueStack
            }
            if (mark != firstMark) {
              p.resetCursorAndValueStack(mark)
              p.valueStack.push(builder.result())
              Rule.matched
            } else Rule.mismatched
          }
        else
          reify {
            val p = c.prefix.splice
            val firstMark = p.markCursorAndValueStack
            var mark = firstMark
            while (op.render().splice.matched)
              mark = p.markCursorAndValueStack
            if (mark != firstMark) {
              p.resetCursorAndValueStack(mark)
              Rule.matched
            } else Rule.mismatched
          }
      reify {
        try block.splice
        catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(RuleFrame.OneOrMore(c.literal(ruleName).splice))
        }
      }
    }
  }

  case class Capture(op: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val mark = p.markCursor
      val result = op.render().splice
      if (result.matched) p.valueStack.push(p.sliceInput(mark))
      result
    }
  }

  // NOTE: applicant might be:
  // - `Function(_, _)` in case of function application
  // - `Ident(_)` in case of case class application
  case class Action(op: OpTree, applicant: Tree, functionType: List[Type]) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = {
      val argTypes = functionType dropRight 1
      val argNames = argTypes.indices map { i ⇒ newTermName("value" + i) }
      val valDefs = (argNames zip argTypes) map { case (n, t) ⇒ q"val $n = p.valueStack.pop().asInstanceOf[$t]" }
      val functionParams = argNames map Ident.apply
      val functionCall = functionType.last match {
        case TypeRef(_, sym, _) if sym.fullName == "scala.Unit" ⇒ q"$applicant(..$functionParams)"
        case _ ⇒ q"p.valueStack.push($applicant(..$functionParams))"
      }
      c.Expr[RuleX] {
        q"""
          val result = ${op.render()}
          if (result.matched) {
            val p = ${c.prefix}
            ..${valDefs.reverse}
            $functionCall
          }
          result
        """
      }
    }
  }

  case class PushAction(arg: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] =
      // for some reason `reify` doesn't seem to work here
      c.Expr[RuleX](q"""
        val p = ${c.prefix}
        p.valueStack.push($arg)
        Rule.matched
      """)
  }

  abstract class Predicate extends OpTree {
    def op: OpTree
    def renderMatch(): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val mark = p.markCursorAndValueStack
      val result = op.render().splice
      p.resetCursorAndValueStack(mark)
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
    def render(ruleName: String): Expr[RuleX] = reify {
      try c.Expr[RuleX](methodCall).splice
      catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.RuleCall(c.literal(ruleName).splice, c.literal(show(methodCall)).splice))
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
