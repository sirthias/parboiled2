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

package org.parboiled

import org.specs2.mutable.Specification

class ParserSpec extends Specification with TestParserComponent {
  "The new parboiled parser" should {
    "successfully recognize single char" in new TestParser {
      def testRule = rule { 'x' }
      beTrue
      parse("x") must Match
      parse("y") must Mismatch
    }

    "successfully recognize valid input - `seq` combinator rule" in new TestParser {
      def testRule = rule { 'a' ~ 'b' }
      parse("ab") must Match
      parse("ac") must Mismatch
      parse("bb") must Mismatch
    }

    "successfully recognize valid input - `firstOf` combinator rule" in new TestParser {
      def testRule = rule { (ch('a') | 'b') }
      parse("a") must Match
      parse("b") must Match
      parse("c") must Mismatch
    }

    "successfully recognize valid input - `zeroOrMore` combinator rule" in new TestParser {
      def testRule = rule { zeroOrMore("a") }
      parse("") must Match
      parse("a") must Match
      parse("aa") must Match
      parse("b") must Match
    }

    "successfully recognize valid input - `oneOrMore` combinator rule" in new TestParser {
      def testRule = rule { oneOrMore("a") }
      parse("") must Mismatch
      parse("a") must Match
      parse("aa") must Match
      parse("b") must Mismatch
    }

    "successfully recognize valid input - `optional` combinator rule" in new TestParser {
      def testRule = rule { optional("a") }
      parse("") must Match
      parse("a") must Match
      parse("aa") must Match
      parse("b") must Match
    }

    "successfully recognize valid input - `not-predicate` combinator rule" in new TestParser {
      def testRule = rule { !"a" }
      parse("") must Match
      parse("a") must Mismatch
      parse("aa") must Mismatch
      parse("b") must Match
    }

    "successfully recognize valid input - `and-predicate` combinator rule" in new TestParser {
      def testRule = rule { &("a") }
      parse("") must Mismatch
      parse("a") must Match
      parse("aa") must Match
      parse("b") must Mismatch
    }

    "successfully recognize EOI" in new TestParser {
      def testRule = rule { EOI }
      parse("") must Match
      parse("x") must Mismatch
    }

    "properly expand string literals to a sequence of char rules" in new TestParser {
      def testRule = rule { "def" }
      parse("def") must Match
      parse("dfe") must Mismatch
    }

    "pass integration tests" in {
      "successfully recognize valid input - combination of rules" in new TestParser {
        def testRule = rule { (ch('a') | 'b' | 'c') ~ (ch('d') | 'e') ~ 'f' ~ EOI }
        parse("adf") must Match
        parse("bdf") must Match
        parse("aef") must Match
        parse("cef") must Match
        parse("adx") must Mismatch
        parse("bbb") must Mismatch
      }

      "successfully recognize valid input - `zeroOrMore` and `seq` combinator rules" in new TestParser {
        def testRule = rule { zeroOrMore("a") ~ zeroOrMore("b") ~ EOI }
        parse("") must Match
        parse("aa") must Match
        parse("b") must Match
        parse("bb") must Match
        parse("ab") must Match
        parse("ba") must Mismatch
      }

      "successfully recognize valid input - `and-predicate` rule sequenced by `charRule` rule" in new TestParser {
        def testRule = rule { &("a") ~ "a" ~ EOI }
        parse("") must Mismatch
        parse("a") must Match
        parse("aa") must Mismatch
        parse("b") must Mismatch
        parse("bb") must Mismatch
      }

      "successfully recognize valid input - `optional` and `seq` combinator rules" in new TestParser {
        def testRule = rule { optional("a") ~ optional("b") ~ EOI }
        parse("") must Match
        parse("aa") must Mismatch
        parse("b") must Match
        parse("bb") must Mismatch
        parse("ab") must Match
        parse("aab") must Mismatch
        parse("abb") must Mismatch
        parse("aabb") must Mismatch
        parse("ba") must Mismatch
      }

      "successfully recognize valid input - `not-predicate` rule sequenced by `charRule` rule" in new TestParser {
        def testRule = rule { !"a" ~ "b" ~ EOI }
        parse("") must Mismatch
        parse("a") must Mismatch
        parse("aa") must Mismatch
        parse("b") must Match
        parse("bb") must Mismatch
      }

      "successfully recognize valid input - `oneOrMore` and `seq` combinator rules" in new TestParser {
        def testRule = rule { oneOrMore("a") ~ oneOrMore("b") ~ EOI }
        parse("") must Mismatch
        parse("aa") must Mismatch
        parse("b") must Mismatch
        parse("bb") must Mismatch
        parse("ab") must Match
        parse("aab") must Match
        parse("abb") must Match
        parse("aabb") must Match
        parse("ba") must Mismatch
      }
    }

    // TODO: Fix this test
    //    "disallow compilation of an illegal string rule" in new TestParser {
    //      CompilerError.verify(
    //        """class TestParser(_input: ParserInput) extends Parser(_input) {
    //                       val string = "def"
    //                       def Illegal = rule { string } // not allowed, strings must be given as literals
    //                     }""",
    //        "Strings in rule definitions have to be literals")
  }
}