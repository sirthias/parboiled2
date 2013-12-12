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

case class ParseError(position: Position, traces: Seq[RuleTrace])

case class Position(index: Int, line: Int, column: Int)

// outermost (i.e. highest-level) rule first
case class RuleTrace(frames: Seq[RuleFrame]) {
  def format: String = frames.map(_.format).filter(_.nonEmpty).mkString(" / ")
}

sealed abstract class RuleFrame {
  import RuleFrame._

  def name: String // the name of rule (method), empty if the rule is anonymous (i.e. an "inner" rule)

  def format: String =
    if (name.nonEmpty) name
    else this match {
      case _: Sequence            ⇒ ""
      case _: FirstOf             ⇒ "|"
      case CharMatch(c, _)        ⇒ "'" + c + '\''
      case StringMatch(s, _)      ⇒ '"' + s + '"'
      case IgnoreCaseChar(c, _)   ⇒ "'" + c + '\''
      case IgnoreCaseString(s, _) ⇒ '"' + s + '"'
      case _: PredicateMatch      ⇒ "<anon predicate>"
      case AnyOf(s, _)            ⇒ '[' + s + ']'
      case _: ANY                 ⇒ "ANY"
      case _: Optional            ⇒ "optional"
      case _: ZeroOrMore          ⇒ "zeroOrMore"
      case _: OneOrMore           ⇒ "oneOrMore"
      case _: Times               ⇒ "times"
      case _: AndPredicate        ⇒ "&"
      case _: NotPredicate        ⇒ "!"
      case _: SemanticPredicate   ⇒ "test"
      case _: RuleCall            ⇒ ""
      case CharRange(from, to, _) ⇒ s"'$from'-'$to'"
    }
}

object RuleFrame {
  case class Sequence(subs: Int, name: String = "") extends RuleFrame
  case class FirstOf(subs: Int, name: String = "") extends RuleFrame
  case class CharMatch(char: Char, name: String = "") extends RuleFrame
  case class StringMatch(string: String, name: String = "") extends RuleFrame
  case class IgnoreCaseChar(char: Char, name: String = "") extends RuleFrame
  case class IgnoreCaseString(string: String, name: String = "") extends RuleFrame
  case class PredicateMatch(predicate: CharPredicate, name: String = "") extends RuleFrame
  case class AnyOf(string: String, name: String = "") extends RuleFrame
  case class ANY(name: String = "") extends RuleFrame
  case class Optional(name: String = "") extends RuleFrame
  case class ZeroOrMore(name: String = "") extends RuleFrame
  case class OneOrMore(name: String = "") extends RuleFrame
  case class Times(min: Int, max: Int, name: String = "") extends RuleFrame
  case class AndPredicate(name: String = "") extends RuleFrame
  case class NotPredicate(name: String = "") extends RuleFrame
  case class SemanticPredicate(name: String = "") extends RuleFrame
  case class RuleCall(calledRule: String, name: String = "") extends RuleFrame
  case class CharRange(from: Char, to: Char, name: String = "") extends RuleFrame
}