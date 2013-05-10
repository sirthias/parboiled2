package org.parboiled.optree

abstract class Expression

case class LiteralChar(val c: Char) extends Expression

case class LiteralString(val c: String) extends Expression

case object EOI extends Expression

case class CharacterClass(val chars: Array[Char]) extends Expression

case class AnyCharacter() extends Expression

case class Grouping(val n: Expression) extends Expression

case class Optional(val n: Expression) extends Expression

case class ZeroOrOne(val n: Expression) extends Expression

case class OneOrMore(val n: Expression) extends Expression

case class AndPredicate(val n: Expression) extends Expression

case class NotPredicate(val n: Expression) extends Expression

case class Sequence(val lhs: Expression, val rhs: Expression) extends Expression

case class FirstOf(val lhs: Expression, val rhs: Expression) extends Expression