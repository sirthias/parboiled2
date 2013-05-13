package org.parboiled
package optree

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  abstract class OpTree {
    def render(): c.universe.Expr[Boolean]
  }

  sealed trait OpTreeCompanion extends PartialFunction[c.Tree, OpTree] {
    val fromTree: PartialFunction[c.Tree, OpTree]
    def isDefinedAt(tree: c.Tree) = fromTree.isDefinedAt(tree)
    def apply(tree: c.Tree) = fromTree.apply(tree)
  }

  object OpTree {
    def apply(tree: c.universe.Tree): OpTree =
      (EOI
        orElse LiteralString
        orElse LiteralChar
        orElse Sequence
        orElse FirstOf //orElse c.abort(c.enclosingPosition, "Invalid rule definition: " + tree)
        )(tree)
  }

  case object EOI extends OpTree with OpTreeCompanion {
    def render(): c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      p.nextChar == p.EOI
    }

    val fromTree: PartialFunction[c.Tree, EOI.type] = {
      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if argTermName.decoded == "EOI" ⇒ EOI
    }
  }

  case class LiteralString(s: String) extends OpTree {
    def render(): c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val cursorPos = p.cursor
      val ts = c.literal(s).splice
      if (ts forall (_ == p.nextChar)) true
      else { p.cursor = cursorPos; false }
    }
  }

  object LiteralString extends OpTreeCompanion {
    val fromTree: PartialFunction[c.Tree, LiteralString] = {
      case Apply(Select(This(typeName), termName), List(Literal(Constant(s: String)))) ⇒ LiteralString(s)
    }
  }

  case class LiteralChar(ch: Char) extends OpTree {
    def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val tc = c.literal(ch).splice
      p.nextChar == tc
    }
  }

  object LiteralChar extends OpTreeCompanion {
    val fromTree: PartialFunction[c.Tree, LiteralChar] = {
      case Apply(Select(This(typeName), termName), List(Literal(Constant(ch: Char)))) ⇒ LiteralChar(ch)
    }
  }

  //  case class CharacterClass(chars: Array[Char]) extends OpTree
  //
  //  case class AnyCharacter() extends OpTree
  //
  //  case class Grouping(n: OpTree) extends OpTree
  //
  //  case class Optional(n: OpTree) extends OpTree
  //
  //  case class ZeroOrOne(n: OpTree) extends OpTree
  //
  //  case class OneOrMore(n: OpTree) extends OpTree
  //
  //  case class AndPredicate(n: OpTree) extends OpTree
  //
  //  case class NotPredicate(n: OpTree) extends OpTree

  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val cursorPos = p.cursor
      if (lhs.render.splice) rhs.render.splice
      else { p.cursor = cursorPos; false }
    }
  }

  object Sequence extends OpTreeCompanion {
    val fromTree: PartialFunction[c.Tree, Sequence] = {
      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$tilde\")" ⇒ {
        val lhs = OpTree(a)
        val rhs = OpTree(arg)
        Sequence(lhs, rhs)
      }
    }
  }

  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val cursorPos = p.cursor
      if (lhs.render.splice) true
      else { p.cursor = cursorPos; rhs.render.splice }
    }
  }

  object FirstOf extends OpTreeCompanion {
    val fromTree: PartialFunction[c.Tree, FirstOf] = {
      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$bar$bar\")" ⇒ {
        val lhs = OpTree(a)
        val rhs = OpTree(arg)
        FirstOf(lhs, rhs)
      }
    }
  }
}
