package org.parboiled2

trait OpTreeContext[OpTreeCtx <: Parser.ParserContext] {
  val c: OpTreeCtx
  import c.universe._

  abstract class OpTree {
    def render(): Expr[Rule]
  }

  object OpTree {
    def apply(tree: Tree): OpTree = tree match {
      case Combinator(x @ (CharacterClass() | Sequence() | FirstOf())) ⇒ x.opTree.get
      case Modifier(x @ (LiteralString() | LiteralChar() | Optional() | ZeroOrMore() | OneOrMore() | AndPredicate())) ⇒ x.opTree.get
      case RuleCall(x) ⇒ x
      case NotPredicate(x) ⇒ x
      case _ ⇒ c.abort(tree.pos, s"Invalid rule definition: $tree\n${showRaw(tree)}")
    }
  }

  object Combinator {
    case class TreeMatch(lhs: Tree, methodName: String, rhs: Tree) { var opTree: Option[OpTree] = None }
    def unapply(tree: Tree): Option[TreeMatch] = tree match {
      case Apply(Select(lhs, Decoded(methodName)), List(rhs)) ⇒ Some(TreeMatch(lhs, methodName, rhs))
      case _ ⇒ None
    }
    abstract class Companion(methodName: String) {
      def apply(lhs: OpTree, rhs: OpTree): OpTree
      def unapply(tm: TreeMatch): Boolean =
        if (tm.methodName == methodName) {
          val lhs = OpTree(tm.lhs)
          val rhs = OpTree(tm.rhs)
          tm.opTree = Some(apply(lhs, rhs))
          true
        } else false
    }
    abstract class CompanionOfStrings(methodName: String) {
      def apply(lhs: LiteralString, rhs: LiteralString): OpTree
      def unapply(tm: TreeMatch): Boolean =
        if (tm.methodName == methodName) {
          (OpTree(tm.lhs), OpTree(tm.rhs)) match {
            case ((lhsStr: LiteralString), (rhsStr: LiteralString)) ⇒
              tm.opTree = Some(apply(lhsStr, rhsStr))
              true
            case _ ⇒ false
          }
        } else false
    }
  }

  object Modifier {
    case class TreeMatch(methodName: String, arg: Tree) { var opTree: Option[OpTree] = None }
    def unapply(tree: Tree): Option[TreeMatch] = tree match {
      case Apply(Select(This(_), Decoded(methodName)), List(arg)) ⇒ Some(TreeMatch(methodName, arg))
      case _ ⇒ None
    }
    abstract class Companion {
      def fromTreeMatch: PartialFunction[TreeMatch, OpTree]
      def unapply(tm: TreeMatch): Boolean =
        // applyOrElse is faster then `isDefined` + `apply`
        fromTreeMatch.applyOrElse(tm, (_: AnyRef) ⇒ null) match {
          case null ⇒ false
          case opTree ⇒
            tm.opTree = Some(opTree)
            true
        }
    }
  }

  case class CharacterClass(lBnd: LiteralString, rBnd: LiteralString) extends OpTree {
    require(lBnd.s.length == 1, "left bound is required to be a single char string")
    require(rBnd.s.length == 1, "right bound is required to be a single char string")
    val lBndChar = lBnd.s.charAt(0)
    val rBndChar = rBnd.s.charAt(0)
    require(lBndChar <= rBndChar, "left bound is required to be less or equal to right bound")

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val nc = p.nextChar()
      val lBndCharSplice = c.literal(lBndChar).splice
      val rBndCharSplice = c.literal(rBndChar).splice
      val matched = lBndCharSplice <= nc && nc <= rBndCharSplice
      if (!matched) {
        if (p.collecting && mark == p.errorMark) {
          p.expectedRules += RuleStack(CharacterClassFrame(lBndCharSplice, rBndCharSplice) +: p.currentCallStack)
        }
        p.reset(mark)
      }
      Rule(matched)
    }
  }
  object CharacterClass extends Combinator.CompanionOfStrings("-")

  // TODO: Having sequence be a simple (lhs, rhs) model causes us to allocate a mark on the stack
  // for every sequence concatenation. If we modeled sequences as a Seq[OpTree] we would be able to
  // reuse a single mutable mark for all intermediate markings in between elements. This will reduce
  // the stack size for all rules with sequences that are more than two elements long.
  case class Sequence(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      if (p.collecting)
        p.currentCallStack.push(SequenceFrame())
      val mark = p.mark
      val lhsSplice = lhs.render().splice
      val res = if (lhsSplice.matched) {
        val rhsSplice = rhs.render().splice
        if (rhsSplice.matched) {
          // success
        } else {
          // rhs failed
          p.reset(mark)
        }
        Rule(rhsSplice.matched)
      } else {
        // lhs failed
        p.reset(mark)
        Rule.failure
      }
      if (p.collecting)
        p.currentCallStack.pop()
      res
    }
  }
  object Sequence extends Combinator.Companion("~")

  case class FirstOf(lhs: OpTree, rhs: OpTree) extends OpTree {
    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      if (p.collecting)
        p.currentCallStack.push(FirstOfFrame())
      val mark = p.mark
      val matched = lhs.render().splice.matched
      val res = if (matched) {
        Rule.success
      } else {
        p.reset(mark)
        val rhsSplice = rhs.render().splice
        if (rhsSplice.matched) {
          // success
        } else {
          // failed
          p.reset(mark)
        }
        Rule(rhsSplice.matched)
      }
      if (p.collecting)
        p.currentCallStack.pop()
      res
    }
  }
  object FirstOf extends Combinator.Companion("|")

  case class LiteralString(s: String) extends OpTree {
    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val ts = c.literal(s).splice
      var ix = 0
      while (ix < ts.length && p.nextChar() == ts.charAt(ix)) ix += 1
      if (ix == ts.length) {
        Rule.success
      } else {
        // failed
        if (p.collecting && mark == p.errorMark) {
          p.expectedRules += RuleStack(StringFrame(s"'${c.literal(s).splice}'") +: p.currentCallStack)
        }
        p.reset(mark)
        Rule.failure
      }
    }
  }
  object LiteralString extends Modifier.Companion {
    // TODO: expand string literal into sequence of LiteralChars for all strings below a certain threshold
    // number of characters (i.e. we "unroll" short strings with, say, less than 16 chars)
    def fromTreeMatch = {
      case Modifier.TreeMatch("str", Literal(Constant(s: String))) ⇒ LiteralString(s)
    }
  }

  case class LiteralChar(ch: Char) extends OpTree {
    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val tc = c.literal(ch).splice
      val matched = p.nextChar() == tc
      if (!matched) {
        // failed
        if (p.collecting && mark == p.errorMark) {
          p.expectedRules += RuleStack(CharFrame(c.literal(ch).splice) +: p.currentCallStack)
        }
        p.reset(mark)
      }
      Rule(matched)
    }
  }
  object LiteralChar extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("ch", Select(This(_), Decoded("EOI"))) ⇒ LiteralChar(Parser.EOI)
      case Modifier.TreeMatch("ch", Literal(Constant(c: Char)))      ⇒ LiteralChar(c)
    }
  }

  case class Optional(op: OpTree) extends OpTree {
    def render(): Expr[Rule] = {
      reify {
        val p = c.prefix.splice
        if (p.collecting)
          p.currentCallStack.push(OptionalFrame())
        val mark = p.mark
        val matched = op.render().splice.matched
        if (!matched) {
          p.reset(mark)
        }
        if (p.collecting)
          p.currentCallStack.pop()
        Rule.success
      }
    }
  }
  object Optional extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("optional", arg) ⇒ Optional(OpTree(arg))
    }
  }

  case class ZeroOrMore(op: OpTree) extends OpTree {
    def render(): Expr[Rule] = {
      reify {
        val p = c.prefix.splice
        if (p.collecting)
          p.currentCallStack.push(ZeroOrMoreFrame())
        var mark = p.mark
        while (op.render().splice.matched) { mark = p.mark }
        p.reset(mark)
        if (p.collecting)
          p.currentCallStack.pop()
        Rule.success
      }
    }
  }
  object ZeroOrMore extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("zeroOrMore", arg) ⇒ ZeroOrMore(OpTree(arg))
    }
  }

  object OneOrMore extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("oneOrMore", arg) ⇒
        val op = OpTree(arg)
        Sequence(op, ZeroOrMore(op))
    }
  }

  abstract class Predicate extends OpTree {
    def op: OpTree
    def renderMatch(): Expr[Boolean] = reify {
      val p = c.prefix.splice
      val mark = p.mark
      val matched = op.render().splice.matched
      p.reset(mark)
      matched
    }
  }

  case class AndPredicate(op: OpTree) extends Predicate {
    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      if (p.collecting)
        p.currentCallStack.push(AndPredicateFrame())
      val matched = renderMatch().splice
      if (p.collecting)
        p.currentCallStack.pop()
      Rule(matched)
    }
  }
  object AndPredicate extends Modifier.Companion {
    def fromTreeMatch = {
      case Modifier.TreeMatch("&", arg: Tree) ⇒ AndPredicate(OpTree(arg))
    }
  }

  case class NotPredicate(op: OpTree) extends Predicate {
    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      if (p.collecting)
        p.currentCallStack.push(NotPredicateFrame())
      val matched = !renderMatch().splice
      if (p.collecting)
        p.currentCallStack.pop()
      Rule(matched)
    }
  }
  object NotPredicate {
    def unapply(tree: Tree): Option[OpTree] = tree match {
      case Apply(Select(arg, Decoded("unary_!")), List()) ⇒ Some(NotPredicate(OpTree(arg)))
      case _ ⇒ None
    }
  }

  case class RuleCall(methodCall: Tree) extends OpTree {
    lazy val rcStr = {
      // NOTE: Decomposition is required
      val Select(This(tpName), termName) = methodCall
      c.literal(s"$tpName.$termName")
    }

    def render(): Expr[Rule] = reify {
      val p = c.prefix.splice
      if (p.collecting)
        p.currentCallStack.push(RuleCallFrame(rcStr.splice))
      val spl = c.Expr[Rule](methodCall).splice
      if (p.collecting)
        p.currentCallStack.pop()
      Rule(spl.matched)
    }
  }
  object RuleCall {
    def unapply(tree: Tree): Option[OpTree] = tree match {
      case x @ Select(This(_), _)           ⇒ Some(RuleCall(x))
      case x @ Apply(Select(This(_), _), _) ⇒ Some(RuleCall(x))
      case _                                ⇒ None
    }
  }

  //  case class AnyCharacter() extends OpTree

  //  case class Grouping(n: OpTree) extends OpTree

  case object Empty extends OpTree {
    def render(): Expr[Rule] = reify {
      Rule.success
    }
  }

  ////////////////// helpers ///////////////////

  private object Decoded {
    def unapply(name: Name): Option[String] = Some(name.decoded)
  }
}
