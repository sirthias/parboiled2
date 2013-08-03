package org.parboiled2

case class ParseError(position: Position, errorRules: Seq[RuleStack])
case class Position(index: Int, line: Int, column: Int)
case class RuleStack(frames: Seq[RuleFrame])

sealed trait RuleFrame {
  def name: String // the name of rule (method), empty if the rule is anonymous (i.e. an "inner" rule)
}

object RuleFrame {
  case class Sequence(name: String = "") extends RuleFrame
  case class CharacterClass(from: Char, to: Char, name: String = "") extends RuleFrame
  case class FirstOf(name: String = "") extends RuleFrame
  case class LiteralString(string: String, name: String = "") extends RuleFrame
  case class LiteralChar(char: Char, name: String = "") extends RuleFrame
  case class RuleCall(calledRule: String, name: String = "") extends RuleFrame
  case class Optional(name: String = "") extends RuleFrame
  case class ZeroOrMore(name: String = "") extends RuleFrame
  case class OneOrMore(name: String = "") extends RuleFrame
  case class AndPredicate(name: String = "") extends RuleFrame
  case class NotPredicate(name: String = "") extends RuleFrame
}
