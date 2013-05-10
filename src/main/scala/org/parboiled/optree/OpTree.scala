package org.parboiled
package optree

import scala.reflect.macros.Context

object OpTree {
  type ParserContext = Context { type PrefixType = Parser }

  def fromAst(c: ParserContext)(e: c.Tree): Expression = {
    import c.universe._

    e match {
      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if argTermName.decoded == "EOI" ⇒
        EOI

      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if termName.decoded == "stringRule" ⇒
        fail("Strings in rule definitions have to be literals")

      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: String)))) ⇒
        LiteralString(x)

      case Apply(Select(This(typeName), termName), List(Literal(Constant(x: Char)))) ⇒
        LiteralChar(x)

      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$tilde\")" ⇒
        Sequence(fromAst(c)(a), fromAst(c)(arg))

      case Apply(Select(a, n), List(arg)) if show(n) == "newTermName(\"$bar$bar\")" ⇒
        FirstOf(fromAst(c)(a), fromAst(c)(arg))

      case x ⇒
        fail("transformToOpTree, unexpected: " + showRaw(x))
    }
  }

  def toAst(c: ParserContext)(e: Expression): c.Expr[Boolean] = {
    import c.universe._

    e match {
      case EOI ⇒ reify {
        val p = c.prefix.splice
        p.nextChar == p.EOI
      }

      case LiteralString(targetString: String) ⇒ reify {
        val p = c.prefix.splice
        val cursorPos = p.cursor
        val ts = c.literal(targetString).splice
        if (ts forall (_ == p.nextChar)) true
        else { p.cursor = cursorPos; false }
      }

      case LiteralChar(targetChar: Char) ⇒ reify {
        val p = c.prefix.splice
        val tc = c.literal(targetChar).splice
        p.nextChar == tc
      }

      case Sequence(lhs: Expression, rhs: Expression) ⇒ reify {
        val p = c.prefix.splice
        val cursorPos = p.cursor
        if (toAst(c)(lhs).splice) toAst(c)(rhs).splice
        else { p.cursor = cursorPos; false }
      }

      case FirstOf(lhs: Expression, rhs: Expression) ⇒ reify {
        val p = c.prefix.splice
        val cursorPos = p.cursor
        if (toAst(c)(lhs).splice) true
        else { p.cursor = cursorPos; toAst(c)(rhs).splice }
      }

      case x ⇒
        fail("transformToAst, unexpected: " + x)
    }
  }

  private def fail(errorMsg: String) = throw new OpTreeException(errorMsg)

  class OpTreeException(_msg: String) extends RuntimeException(_msg)
}
