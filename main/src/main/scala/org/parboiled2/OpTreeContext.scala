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
    def sequencerIsRule1(sequencer: Tree, pos: Position) = sequencer match {
      case q"parboiled2.this.Sequencer.forReduction[$l, $r]" => false
      case q"parboiled2.this.Sequencer.forRule0" ⇒ false
      case q"parboiled2.this.Sequencer.forRule1[$t]" ⇒ true
      case _ ⇒ c.abort(pos, s"Unexpected type of `Sequencer`: ${show(sequencer)}")
    }

    // TODO: DRY-up once we have the first complete implementation of all DSL elements
    def apply(tree: Tree): OpTree =
      tree match {
        case Apply(Apply(TypeApply(Select(lhs, Decoded("~")), _), List(rhs)), _)          ⇒ Sequence(OpTree(lhs), OpTree(rhs))

        case Apply(TypeApply(Select(lhs, Decoded("|")), _), List(rhs))                    ⇒ FirstOf(OpTree(lhs), OpTree(rhs))

        case Apply(Select(This(_), Decoded("str")), List(Literal(Constant(s: String))))   ⇒ LiteralString(s)

        case Apply(Select(This(_), Decoded("ch")), List(Select(This(_), Decoded("EOI")))) ⇒ LiteralChar(EOI)

        case Apply(Select(This(_), Decoded("ch")), List(Literal(Constant(c: Char))))      ⇒ LiteralChar(c)

        case Apply(Apply(TypeApply(Select(This(_), Decoded("optional")), _), List(arg)), List(optionalizer)) ⇒ {
          val opIsRule1 = optionalizer match {
            case q"parboiled2.this.Optionalizer.forReduction[$l, $r]" ⇒ false
            case q"parboiled2.this.Optionalizer.forRule0" ⇒ false
            case q"parboiled2.this.Optionalizer.forRule1[$t]" ⇒ true
            case _ ⇒ c.abort(tree.pos, s"Unexpected type of `Optionalizer`: ${show(optionalizer)}")
          }
          Optional(OpTree(arg), opIsRule1)
        }

        case Apply(Apply(TypeApply(Select(This(_), Decoded("zeroOrMore")), _), List(arg)), List(sequencer)) ⇒ {
          val opIsRule1 = sequencerIsRule1(sequencer, tree.pos)
          ZeroOrMore(OpTree(arg), opIsRule1)
        }

        case Apply(Apply(TypeApply(Select(This(_), Decoded("oneOrMore")), _), List(arg)), List(sequencer)) ⇒ {
          val opIsRule1 = sequencerIsRule1(sequencer, tree.pos)
          OneOrMore(OpTree(arg), opIsRule1)
        }

        case Apply(Apply(TypeApply(Select(This(_), Decoded("capture")), _), List(arg)), _) ⇒ Capture(OpTree(arg))

        case Apply(Select(This(_), Decoded("&")), List(arg)) ⇒ AndPredicate(OpTree(arg))

        case x @ Select(This(_), _) ⇒ RuleCall(x)
        case x @ Apply(Select(This(_), _), _) ⇒ RuleCall(x)

        case Apply(Select(arg, Decoded("unary_!")), List()) ⇒ NotPredicate(OpTree(arg))

        case Apply(Apply(TypeApply(Select(Select(Apply(Apply(TypeApply(Select(This(_), Decoded("pimpActionOp")), _),
          List(r)), _), Decoded("~>")), Decoded("apply")), _), List(f @ Function(_, _))), _) ⇒ {
          val argsTypes = f.tpe match {
            case TypeRef(_, _, typeArgs) ⇒ typeArgs
            case _                       ⇒ c.abort(tree.pos, s"Unexpected function.tpe: ${f.tpe}\n${showRaw(f.tpe)}")
          }
          Action(OpTree(r), f, argsTypes)
        }

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

  case class Optional(op: OpTree, opIsRule1: Boolean) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = {
      reify {
        try {
          val p = c.prefix.splice
          val savedTop = p.valueStack.top
          val inputStartMark = p.inputStartMark
          if (op.render().splice.matched) {
            c.Expr[Unit](if (opIsRule1) q"p.valueStack.push(Some(p.valueStack.pop())) " else q"()").splice
          } else {
            c.Expr[Unit](if (opIsRule1) q"p.valueStack.push(None)" else q"()").splice
            p.resetInputStartMark(inputStartMark)
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
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        var mark = p.mark
        c.Expr[Unit](
          if (opIsRule1) q"""
            // TODO: If `scala.collection.immutable` is omitted then `scalac`
            // can't find `VectorBuilder` type. Seems like `macro paradise` bug
            val builder = new scala.collection.immutable.VectorBuilder[Any]
            while (${op.render()}.matched) {
              builder += p.valueStack.pop()
              mark = p.mark
            }
            p.reset(mark)
            p.valueStack.push(builder.result())
          """
          else q"""
            while (${op.render()}.matched)
              mark = p.mark
            p.reset(mark)
          """
        ).splice
        Rule.matched
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.ZeroOrMore(c.literal(ruleName).splice))
      }
    }
  } // """ -- stub comment to bring IDE styling to life

  case class OneOrMore(op: OpTree, opIsRule1: Boolean) extends OpTree {
    // TODO: DRY up with `ZeroOrMore#render` implementation
    def render(ruleName: String): Expr[RuleX] = reify {
      try {
        val p = c.prefix.splice
        val firstMark = p.mark
        var mark = firstMark
        c.Expr[Unit](
          if (opIsRule1) q"""
            // TODO: If `scala.collection.immutable` is omitted then `scalac`
            // can't find `VectorBuilder` type. Seems like `macro paradise` bug
            val builder = new scala.collection.immutable.VectorBuilder[Any]
            while (${op.render()}.matched) {
              builder += p.valueStack.pop()
              mark = p.mark
            }
            if (mark != firstMark)
              p.valueStack.push(builder.result())
          """ else q"""
            while (${op.render()}.matched)
              mark = p.mark
          """
        ).splice // """ -- stub comment to bring IDE styling to life
        p.reset(mark)
        Rule(mark != firstMark)
      } catch {
        case e: Parser.CollectingRuleStackException ⇒
          e.save(RuleFrame.OneOrMore(c.literal(ruleName).splice))
      }
    }
  }

  case class Capture(op: OpTree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = reify {
      val p = c.prefix.splice
      val inputStartMark = p.inputStartMark
      val result = op.render().splice
      if (result.matched) {
        p.valueStack.push(p.sliceInput(inputStartMark))
      }
      result
    }
  }

  case class Action(op: OpTree, function: Function, functionType: List[Type]) extends OpTree {
    // NOTE: Kudos to http://stackoverflow.com/users/931323/ghik for these bits
    // http://stackoverflow.com/questions/18486977/how-to-pass-arguments-to-a-function-of-arbitrary-number-of-parameters-via-macro

    val functionTypeReversed = functionType.reverse // t1, t2, t3 -> r
    val argsTypes = functionTypeReversed drop 1 // t3, t2, t1
    val argNames = (argsTypes.size to 1 by -1) map (i ⇒ newTermName(s"value$i")) // v3, v2, v1
    val args = argNames map (Ident(_))
    val valDefs = (argsTypes zip argNames) map { // (t3, v3), (t2, v2) (t1, v1)
      case (argType, argName) ⇒ q"val $argName = p.valueStack.pop().asInstanceOf[$argType]"
    }
    val functionResultType = functionTypeReversed.head

    def render(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
        val p = ${c.prefix}
        val result = ${op.render()}
        if (result.matched) {
          ..$valDefs
          ${
            c.Expr[Unit](functionResultType match {
              case TypeRef(_, sym, _) if sym.fullName == "scala.Unit" => q"$function(..${args.reverse})"
              case _ => q"p.valueStack.push($function(..${args.reverse}))"
            })
          }
        }
        result
      """
    )
  }

  case class PushAction(arg: Tree) extends OpTree {
    def render(ruleName: String): Expr[RuleX] = c.Expr[RuleX](q"""
      val p = ${c.prefix}
      p.valueStack.push($arg)
      Rule.matched
    """)
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
