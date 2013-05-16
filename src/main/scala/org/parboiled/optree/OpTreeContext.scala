package org.parboiled
package optree

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  type FromTree[T <: OpTree] = PartialFunction[Tree, T]

  object Decoded {
    def unapply(name: Name): Option[String] = Some(name.decoded)
  }

  abstract class OpTree {
    def render(): Expr[Boolean]
  }

  sealed trait OpTreeCompanion extends FromTree[OpTree] {
    val fromTree: FromTree[OpTree]
    def isDefinedAt(tree: Tree) = fromTree.isDefinedAt(tree)
    def apply(tree: Tree) = fromTree.apply(tree)
  }

  object OpTree extends OpTreeCompanion {
    val fromTree: FromTree[OpTree] = {
      (LiteralString
        orElse LiteralChar
        orElse Sequence
        orElse FirstOf
        orElse { case x ⇒ c.abort(c.enclosingPosition, "Invalid rule definition: " + x) })
    }
  }

  case class LiteralString(s: String) extends OpTree {
    def render(): Expr[Boolean] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val ts = c.literal(s).splice
      var ix = 0
      while (ix < ts.length && p.nextChar() == ts.charAt(ix)) ix += 1
      if (ix == ts.length) true
      else {
        p.reset(mark)
        false
      }
    }
  }

  object LiteralString extends OpTreeCompanion {
    // TODO: expand string literal into sequence of LiteralChars for all strings below a certain threshold
    // number of characters (i.e. we "unroll" short strings with, say, less than 16 chars)

    val fromTree: FromTree[LiteralString] = {
      case Apply(Select(This(_), Decoded("stringRule")), List(Literal(Constant(s: String)))) ⇒ LiteralString(s)
    }
  }

  case class LiteralChar(ch: Char) extends OpTree {
    def render(): Expr[Boolean] = reify {
      val p = c.prefix.splice
      val tc = c.literal(ch).splice
      p.nextChar() == tc
    }
  }

  object LiteralChar extends OpTreeCompanion {
    val fromTree: FromTree[LiteralChar] = {
      case Apply(Select(This(_), Decoded("charRule")), List(Select(This(_), Decoded("EOI")))) ⇒ LiteralChar(Parser.EOI)
      case Apply(Select(This(_), Decoded("charRule")), List(Literal(Constant(ch: Char))))     ⇒ LiteralChar(ch)
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

  // TODO: Having sequence be a simple (lhs, rhs) model causes us to allocate a mark on the stack
  // for every sequence concatenation. If we modeled sequences as a Seq[OpTree] we would be able to
  // reuse a single mutable mark for all intermediate markings in between elements. This will reduce
  // the stack size for all rules with sequences that are more than two elements long.
  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(): Expr[Boolean] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      if (lhs.render().splice) rhs.render().splice
      else { p.reset(mark); false }
    }
  }

  object Sequence extends OpTreeCompanion {
    val fromTree: FromTree[Sequence] = {
      case Apply(Select(a, Decoded("~")), List(arg)) ⇒ {
        val lhs = OpTree(a)
        val rhs = OpTree(arg)
        Sequence(lhs, rhs)
      }
    }
  }

  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(): Expr[Boolean] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      if (lhs.render().splice) true
      else { p.reset(mark); rhs.render().splice }
    }
  }

  object FirstOf extends OpTreeCompanion {
    val fromTree: FromTree[FirstOf] = {
      case Apply(Select(a, Decoded("||")), List(arg)) ⇒ {
        val lhs = OpTree(a)
        val rhs = OpTree(arg)
        FirstOf(lhs, rhs)
      }
    }
  }
}
