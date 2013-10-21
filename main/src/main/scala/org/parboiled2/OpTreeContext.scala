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
        case q"$a.this.test($flag)"                           ⇒ SemanticPredicate(flag)
        case q"$a.this.optional[$b, $c]($arg)($optionalizer)" ⇒ Optional(OpTree(arg), isForRule1(optionalizer))
        case q"$a.this.zeroOrMore[$b, $c]($arg)($sequencer)"  ⇒ ZeroOrMore(OpTree(arg), isForRule1(sequencer))
        case q"$a.this.oneOrMore[$b, $c]($arg)($sequencer)"   ⇒ OneOrMore(OpTree(arg), isForRule1(sequencer))
        case q"$a.this.capture[$b, $c]($arg)($d)"             ⇒ Capture(OpTree(arg))
        case q"$a.this.&($arg)"                               ⇒ AndPredicate(OpTree(arg))
        case q"$a.this.ANY"                                   ⇒ AnyChar
        case q"$a.this.EMPTY"                                 ⇒ Empty
        case q"$a.this.nTimes[$ti, $to]($times, $r, $sep)($sequencer)" ⇒
          NTimes(times, OpTree(r), tree.pos, isForRule1(sequencer), sep)
        case q"$a.this.$b"       ⇒ RuleCall(tree)
        case q"$a.this.$b(..$c)" ⇒ RuleCall(tree)
        case q"$a.unary_!()"     ⇒ NotPredicate(OpTree(a))
        case q"$a.this.pimpActionOp[$b1, $b2]($r)($ops).~>.apply[..$e]($f)($g, parboiled2.this.Capture.capture[$ts])" ⇒
          Action(OpTree(r), f, ts.tpe.asInstanceOf[TypeRef].args)
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
        val mark = p.__markCursorAndValueStack
        val left = lhs.render().splice
        if (left.matched) left
        else {
          p.__resetCursorAndValueStack(mark)
          rhs.render().splice
        }
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.FirstOf(c.literal(ruleName).splice))
      }
    }
  }

  /**
   * @param times is greater than zero in `render` (see `object NTimes.apply`)
   * @param rule to match `times` times
   * @param separator rule that is between matching `rule`
   * @param opIsRule1 a flag whether `rule` returns a result or not
   */
  case class NTimes(times: Tree, rule: OpTree, opIsRule1: Boolean, separator: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val timez = c.Expr[Int](times).splice
      if (timez == 0) {
        c.Expr[Unit](if (opIsRule1) q"${c.prefix}.__valueStack.push(Vector())" else q"()").splice
        Rule.matched
      } else {
        try {
          val p = c.prefix.splice
          var matching = true
          var ix = 0
          val mark = p.__markCursorAndValueStack
          c.Expr[Unit](if (opIsRule1) q"val builder = new scala.collection.immutable.VectorBuilder[Any]" else q"()").splice
          while (matching && ix < timez) {
            val sepMatched = ix == 0 || separator.render().splice.matched
            if (sepMatched) {
              val rl = rule.render().splice
              if (rl.matched) {
                ix += 1
                c.Expr[Unit](if (opIsRule1) q"builder += p.__valueStack.pop()" else q"()").splice
              } else {
                matching = false
              }
            } else {
              matching = false
            }
          }
          if (matching) {
            c.Expr[Unit](if (opIsRule1) q"p.__valueStack.push(builder.result())" else q"()").splice
            Rule.matched
          } else {
            p.__resetCursorAndValueStack(mark)
            p.__onCharMismatch()
            Rule.mismatched
          }
        } catch {
          case e: Parser.CollectingRuleStackException ⇒
            e.save(RuleFrame.NTimes(timez, c.literal(show(rule)).splice,
              c.literal(show(separator)).splice, c.literal(ruleName).splice))
        }
      }
    }
  }
  object NTimes {
    def apply(times: Tree, rule: OpTree, pos: Position, opIsRule1: Boolean, separator: Tree): OpTree =
      times match {
        case Literal(Constant(timez: Int)) if timez < 0 ⇒ c.abort(pos, "`times` must be non-negative")
        case _ ⇒
          val separatorOpTree = separator match {
            case q"$a.this.nTimes$$default$$3[$ti, $to]" ⇒ Empty
            case _                                       ⇒ OpTree(separator)
          }
          NTimes(times, rule, opIsRule1, separatorOpTree)
      }
  }

  case class SemanticPredicate(flagTree: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
      try {
        val p = ${c.prefix}
        Rule($flagTree || p.__onCharMismatch())
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.SemanticPredicate($ruleName))
      }
    """)
  }

  case class LiteralString(stringTree: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val string = c.Expr[String](stringTree).splice
      try {
        val p = c.prefix.splice
        var ix = 0
        while (ix < string.length && p.__nextChar() == string.charAt(ix)) ix += 1
        Rule(ix == string.length || p.__onCharMismatch())
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
        Rule(p.__nextChar() == char || p.__onCharMismatch())
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
        Rule(p.__nextChar() != EOI || p.__onCharMismatch())
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
          val mark = p.__markCursor
          if (op.render().splice.matched) {
            c.Expr[Unit](if (opIsRule1) q"p.__valueStack.push(Some(p.__valueStack.pop())) " else q"()").splice
          } else {
            c.Expr[Unit](if (opIsRule1) q"p.__valueStack.push(None)" else q"()").splice
            p.__resetCursor(mark)
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
            var mark = p.__markCursorAndValueStack
            val builder = new VectorBuilder[Any]
            while (op.render().splice.matched) {
              builder += p.__valueStack.pop()
              mark = p.__markCursorAndValueStack
            }
            p.__resetCursorAndValueStack(mark)
            p.__valueStack.push(builder.result())
          }
        else
          reify {
            val p = c.prefix.splice
            var mark = p.__markCursorAndValueStack
            while (op.render().splice.matched)
              mark = p.__markCursorAndValueStack
            p.__resetCursorAndValueStack(mark)
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
            val firstMark = p.__markCursorAndValueStack
            var mark = firstMark
            val builder = new VectorBuilder[Any]
            while (op.render().splice.matched) {
              builder += p.__valueStack.pop()
              mark = p.__markCursorAndValueStack
            }
            if (mark != firstMark) {
              p.__resetCursorAndValueStack(mark)
              p.__valueStack.push(builder.result())
              Rule.matched
            } else Rule.mismatched
          }
        else
          reify {
            val p = c.prefix.splice
            val firstMark = p.__markCursorAndValueStack
            var mark = firstMark
            while (op.render().splice.matched)
              mark = p.__markCursorAndValueStack
            if (mark != firstMark) {
              p.__resetCursorAndValueStack(mark)
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
      val mark = p.__markCursor
      val result = op.render().splice
      if (result.matched) p.__valueStack.push(p.__sliceInput(mark))
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
        Rule.matched
      """)
    }
  }

  abstract class Predicate extends OpTree {
    def op: OpTree
    def renderMatch(): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val mark = p.__markCursorAndValueStack
      val result = op.render().splice
      p.__resetCursorAndValueStack(mark)
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
        val char = p.__nextChar()
        val ok = c.literal(lowerBound).splice <= char && char <= c.literal(upperBound).splice
        Rule(ok || p.__onCharMismatch())
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
