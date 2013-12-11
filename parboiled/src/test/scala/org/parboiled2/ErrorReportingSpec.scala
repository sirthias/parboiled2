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

class ErrorReportingSpec extends TestParserSpec {
  import RuleFrame._

  "The Parser should properly report errors" >> {

    "example 1" in new TestParser0 {
      def targetRule = rule { oneOrMore("a") ~ 'b' ~ anyOf("cde") ~ "fgh" ~ EOI }

      "" must beMismatchedWithErrorMsg(
        """Unexpected end of input, expected 'a' (line 1, column 1):
          |
          |^
          |
          |Mismatched rules at error location:
          |  targetRule / oneOrMore / 'a'
          |""")

      "ax" must beMismatchedWithErrorMsg(
        """Invalid input 'x', expected 'a' or 'b' (line 1, column 2):
          |ax
          | ^
          |
          |Mismatched rules at error location:
          |  targetRule / oneOrMore / 'a'
          |  targetRule / 'b'
          |""")

      "abx" must beMismatchedWithErrorMsg(
        """Invalid input 'x', expected [cde] (line 1, column 3):
          |abx
          |  ^
          |
          |Mismatched rules at error location:
          |  targetRule / [cde]
          |""")

      "abcfx" must beMismatchedWithErrorMsg(
        """Invalid input 'x', expected 'g' (line 1, column 5):
          |abcfx
          |    ^
          |
          |Mismatched rules at error location:
          |  targetRule / "fgh" / 'g'
          |""")
    }

    //    "compose error messages for simple calculator" in new SimpleCalculator {
    //      def targetRule = InputLine
    //
    //      "3+*5" must beMismatchedWithError(
    //        ParseError(Position(2, 1, 3), Seq(
    //          RuleStack(Seq(CharacterClass('0', '9', "Digit"), RuleCall("", "SimpleCalculator.this.Digit"), OneOrMore("Digits"),
    //            RuleCall("", "SimpleCalculator.this.Digits"), FirstOf("Factor"), RuleCall("", "SimpleCalculator.this.Factor"),
    //            Sequence("Term"), RuleCall("", "SimpleCalculator.this.Term"), Sequence(),
    //            ZeroOrMore(), Sequence("Expression"), RuleCall("", "SimpleCalculator.this.Expression"), Sequence("InputLine"))),
    //          RuleStack(Seq(LiteralString("(", ""), Sequence(), Sequence("Parens"), RuleCall("", "SimpleCalculator.this.Parens"),
    //            FirstOf("Factor"), RuleCall("", "SimpleCalculator.this.Factor"), Sequence("Term"), RuleCall("", "SimpleCalculator.this.Term"),
    //            Sequence(), ZeroOrMore(), Sequence("Expression"), RuleCall("", "SimpleCalculator.this.Expression"), Sequence("InputLine"))))))
    //    }
    //
    //    "track lines numbers" in {
    //      "first line" in new TestParser0 {
    //        def targetRule = rule { str("a\n") ~ "b\n" ~ "c" }
    //
    //        "x\nb\nc" must beMismatchedWithError(ParseError(Position(0, 1, 1), Seq(
    //          RuleStack(Seq(LiteralString("a\n"), Sequence(), Sequence("targetRule"))))))
    //      }
    //
    //      "second line" in new TestParser0 {
    //        def targetRule = rule { str("a\n") ~ "b\n" ~ "c" }
    //
    //        "a\nx\nc" must beMismatchedWithError(ParseError(Position(2, 2, 1), Seq(
    //          RuleStack(Seq(LiteralString("b\n"), Sequence(), Sequence("targetRule"))))))
    //      }
    //
    //      "third line" in new TestParser0 {
    //        def targetRule = rule { str("a\n") ~ "b\n" ~ "c" }
    //
    //        "a\nb\nx" must beMismatchedWithError(ParseError(Position(4, 3, 1), Seq(
    //          RuleStack(Seq(LiteralString("c"), Sequence("targetRule"))))))
    //      }
    //    }
    //
    //    "correctly process FirstOf" in {
    //      "producing no errors for first alternative" in new TestParser0 {
    //        def targetRule = rule { ch('a') | 'b' }
    //
    //        "aaa" must beMatched
    //      }
    //
    //      "producing no errors for second alternative" in new TestParser0 {
    //        def targetRule = rule { ch('a') | 'b' }
    //
    //        "b" must beMatched
    //      }
    //    }
  }
}