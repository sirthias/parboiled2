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

class ErrorReportingSpec extends SimpleCalculatorSpec {
  import RuleFrame._

  "Error reporting" should {
    "compose error messages for simple expression" in new TestParser {
      def targetRule = rule { oneOrMore("x") ~ ch('a') ~ 'b' ~ 'e' }

      parse("xxxace") ===
        Left(ParseError(Position(4, 1, 5), Seq(RuleStack(Seq(LiteralChar('b'), Sequence(), Sequence("targetRule"))))))
    }

    "compose error messages for simple calculator" in new SimpleCalculator {
      def targetRule = InputLine

      "3+*5" must beMismatchedWithError(
        ParseError(Position(2, 1, 3), Seq(
          RuleStack(Seq(CharacterClass('0','9',"Digit"), RuleCall("","SimpleCalculator.Digit"), Sequence("Digits"),
            RuleCall("","SimpleCalculator.Digits"), FirstOf("Factor"), RuleCall("","SimpleCalculator.Factor"),
            Sequence("Term"), RuleCall("","SimpleCalculator.Term"), Sequence(),
            ZeroOrMore(), Sequence("Expression"), RuleCall("","SimpleCalculator.Expression"), Sequence("InputLine"))),
          RuleStack(Seq(LiteralString("(",""), Sequence(), Sequence("Parens"), RuleCall("","SimpleCalculator.Parens"),
            FirstOf("Factor"), RuleCall("","SimpleCalculator.Factor"), Sequence("Term"),
            RuleCall("","SimpleCalculator.Term"), Sequence(), ZeroOrMore(), Sequence("Expression"),
            RuleCall("","SimpleCalculator.Expression"), Sequence("InputLine"))))))
    }

    "track lines numbers" in {
      "first line" in new TestParser {
        def targetRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }

        "x\nb\nc" must beMismatchedWithError(ParseError(Position(0, 1, 1), Seq(
          RuleStack(Seq(LiteralString("a\n"), Sequence(), Sequence("targetRule")))
        )))
      }

      "second line" in new TestParser {
        def targetRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }

        "a\nx\nc" must beMismatchedWithError(ParseError(Position(2, 2, 1), Seq(
          RuleStack(Seq(LiteralString("b\n"), Sequence(), Sequence("targetRule")))
        )))
      }

      "third line" in new TestParser {
        def targetRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }

        "a\nb\nx" must beMismatchedWithError(ParseError(Position(4, 3, 1), Seq(
          RuleStack(Seq(LiteralString("c"), Sequence("targetRule")))
        )))
      }
    }

    "correctly process FirstOf" in {
      "producing no errors for first alternative" in new TestParser {
        def targetRule: Rule = rule { ch('a') | 'b' }

        "aaa" must beMatched
      }

      "producing no errors for second alternative" in new TestParser {
        def targetRule: Rule = rule { ch('a') | 'b' }

        "b" must beMatched
      }
    }
  }
}