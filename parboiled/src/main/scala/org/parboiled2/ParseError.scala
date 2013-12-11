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

case class ParseError(position: Position, errorRules: Seq[RuleStack])
case class Position(index: Int, line: Int, column: Int)
case class RuleStack(frames: Seq[RuleFrame])

sealed trait RuleFrame {
  def name: String // the name of rule (method), empty if the rule is anonymous (i.e. an "inner" rule)
}

object RuleFrame {
  case class Sequence(name: String = "") extends RuleFrame
  case class FirstOf(name: String = "") extends RuleFrame
  case class CharMatch(char: Char, name: String = "") extends RuleFrame
  case class StringMatch(string: String, name: String = "") extends RuleFrame
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
  case class CharacterRange(from: Char, to: Char, name: String = "") extends RuleFrame
}
