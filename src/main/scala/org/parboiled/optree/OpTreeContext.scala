package org.parboiled
package optree

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  abstract class OpTree {
    def render(): c.Expr[Boolean]
  }

  object OpTree {

    // format: OFF
    def apply(tree: c.Tree): OpTree = (
      //    EOI(c, tree) orElse
      //    LiteralString(c, tree) // orElse ...
      //    getOrElse c.abort(c.enclosingPosition, "Invalid rule definition")
      LiteralString(tree)
      )
    // format: ON
  }

  //  case object EOI extends OpTree {
  //    def render(): c.Expr[Boolean] = ???
  //
  //    def apply[OpTreeCtx](c: OpTreeCtx): PartialFunction[c.Tree, EOI.type] = {
  //      import c.universe._
  //      case Apply(Select(This(typeName), termName), List(Select(This(argTypeName), argTermName))) if argTermName.decoded == "EOI" ⇒
  //      EOI
  //    }
  //  }

  case class LiteralString(s: String) extends OpTree {
    def render: c.Expr[Boolean] = ???
  }

  object LiteralString {
    def apply: PartialFunction[c.Tree, LiteralString] =
      {
        case Apply(Select(This(typeName), termName), List(Literal(Constant(s: String)))) ⇒ LiteralString(s)
      }
  }

  //  case class LiteralChar(c: Char) extends OpTree
  //
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
  //
  //  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree
  //
  //  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree
}
