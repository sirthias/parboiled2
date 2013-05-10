package org.parboiled
package optree

class OpTreeContext[OpTreeCtx <: Parser.ParserContext](val c: OpTreeCtx) {
  import c.universe._

  abstract class Expression {
    def render: c.universe.Expr[Boolean] = ???
  }

  case class LiteralChar(val x: Char) extends Expression {
    override def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val tc = c.literal(x).splice
      p.nextChar == tc
    }
  }

  case class LiteralString(val s: String) extends Expression {
    override def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val cursorPos = p.cursor
      val ts = c.literal(s).splice
      if (ts forall (_ == p.nextChar)) true
      else { p.cursor = cursorPos; false }
    }
  }

  case object EOI extends Expression {
    override def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      p.nextChar == p.EOI
    }

    def unapply(tree: c.universe.Tree): Option[EOI.type] = tree match {
      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if argTermName.decoded == "EOI" ⇒
        Some(EOI)
      case _ ⇒ None
    }
  }

  case class CharacterClass(val chars: Array[Char]) extends Expression

  case class AnyCharacter() extends Expression

  case class Grouping(val n: Expression) extends Expression

  case class Optional(val n: Expression) extends Expression

  case class ZeroOrOne(val n: Expression) extends Expression

  case class OneOrMore(val n: Expression) extends Expression

  case class AndPredicate(val n: Expression) extends Expression

  case class NotPredicate(val n: Expression) extends Expression

  case class Sequence(val lhs: Expression, val rhs: Expression) extends Expression {
    override def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val cursorPos = p.cursor
      if (lhs.render.splice) rhs.render.splice
      else { p.cursor = cursorPos; false }
    }
  }

  case class FirstOf(val lhs: Expression, val rhs: Expression) extends Expression {
    override def render: c.universe.Expr[Boolean] = reify {
      val p = c.prefix.splice
      val cursorPos = p.cursor
      if (lhs.render.splice) true
      else { p.cursor = cursorPos; rhs.render.splice }
    }
  }

  object LiteralChar {
    def unapply(tree: c.universe.Tree): Option[LiteralChar] = tree match {
      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: Char)))) ⇒ Some(LiteralChar(x))
      case _ ⇒ None
    }
  }

  object LiteralString {
    def unapply(tree: c.universe.Tree): Option[LiteralString] = tree match {
      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: String)))) ⇒ Some(LiteralString(x))
      case _ ⇒ None
    }
  }

  object Sequence {
    def unapply(tree: c.universe.Tree): Option[Sequence] = tree match {
      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$tilde\")" ⇒
        Some(Sequence(parse(a), parse(arg)))
      case _ ⇒ None
    }
  }

  object FirstOf {
    def unapply(tree: c.universe.Tree): Option[FirstOf] = tree match {
      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$bar$bar\")" ⇒
        Some(FirstOf(parse(a), parse(arg)))
      case _ ⇒ None
    }
  }

  def parse(tree: c.universe.Tree): Expression = tree match {
    case EOI(x)           ⇒ x
    case LiteralString(x) ⇒ x
    case LiteralChar(x)   ⇒ x
    case Sequence(x)      ⇒ x
    case FirstOf(x)       ⇒ x
    case x                ⇒ fail("OpTree.parse, unexpected: " + x)
  }

  private def fail(errorMsg: String) = throw new OpTreeException(errorMsg)

  class OpTreeException(_msg: String) extends RuntimeException(_msg)
}