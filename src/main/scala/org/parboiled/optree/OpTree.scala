package org.parboiled.optree

abstract class Node

case class LiteralString(val s: String) extends Node

case class CharacterClass(val chars: Array[Char]) extends Node

case class AnyCharacter() extends Node

case class Grouping(val n: Node) extends Node

case class Optional(val n: Node) extends Node

case class ZeroOrOne(val n: Node) extends Node

case class OneOrMore(val n: Node) extends Node

case class AndPredicate(val n: Node) extends Node

case class NotPredicate(val n: Node) extends Node

case class Sequence(val lhs: Node, val rhs: Node) extends Node

case class PrioritisedChoice(val lhs: Node, val rhs: Node) extends Node