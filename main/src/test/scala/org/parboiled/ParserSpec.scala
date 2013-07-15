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

class ParserSpec extends TestParserSpec {
  "The new parboiled parser" should {
    "successfully recognize single char" in new TestParser {
      def testRule = rule { 'x' }
      "x" must Match
      "y" must Mismatch
    }

    "successfully recognize valid input - `seq` combinator rule" in new TestParser {
      def testRule = rule { 'a' ~ 'b' }
      "ab" must Match
      "ac" must Mismatch
      "bb" must Mismatch
    }

    "successfully recognize valid input - `firstOf` combinator rule" in new TestParser {
      def testRule = rule { (ch('a') | 'b') }
      "a" must Match
      "b" must Match
      "c" must Mismatch
    }

    "successfully recognize valid input - `zeroOrMore` combinator rule" in new TestParser {
      def testRule = rule { zeroOrMore("a") }
      "a" must Match
      "aa" must Match
      "b" must Match
    }

    "successfully recognize valid input - `oneOrMore` combinator rule" in new TestParser {
      def testRule = rule { oneOrMore("a") }
      "a" must Match
      "aa" must Match
      "b" must Mismatch
    }

    "successfully recognize valid input - `optional` combinator rule" in new TestParser {
      def testRule = rule { optional("a") }
      "" must Match
      "a" must Match
      "b" must Match
    }

    "successfully recognize valid input - `not-predicate` combinator rule" in new TestParser {
      def testRule = rule { !"a" }
      "" must Match
      "a" must Mismatch
      "aa" must Mismatch
      "b" must Match
    }

    "successfully recognize valid input - `and-predicate` combinator rule" in new TestParser {
      def testRule = rule { &("a") }
      "a" must Match
      "aa" must Match
      "b" must Mismatch
    }

    "successfully recognize EOI" in new TestParser {
      def testRule = rule { EOI }
      "" must Match
      "x" must Mismatch
    }

    "properly expand string literals to a sequence of char rules" in new TestParser {
      def testRule = rule { "def" }
      "def" must Match
      "dfe" must Mismatch
    }

    "pass integration tests" in {
      "successfully recognize valid input - combination of rules" in new TestParser {
        def testRule = rule { (ch('a') | 'b' | 'c') ~ (ch('d') | 'e') ~ 'f' ~ EOI }
        "adf" must Match
        "bdf" must Match
        "aef" must Match
        "cef" must Match
        "adx" must Mismatch
        "bbb" must Mismatch
      }

      "successfully recognize valid input - `zeroOrMore` and `seq` combinator rules" in new TestParser {
        def testRule = rule { zeroOrMore("a") ~ zeroOrMore("b") ~ EOI }
        "" must Match
        "aa" must Match
        "b" must Match
        "bb" must Match
        "ab" must Match
        "ba" must Mismatch
      }

      "successfully recognize valid input - `and-predicate` rule sequenced by `charRule` rule" in new TestParser {
        def testRule = rule { &("a") ~ "a" ~ EOI }
        "" must Mismatch
        "a" must Match
        "aa" must Mismatch
        "b" must Mismatch
        "bb" must Mismatch
      }

      "successfully recognize valid input - `optional` and `seq` combinator rules" in new TestParser {
        def testRule = rule { optional("a") ~ optional("b") ~ EOI }
        "" must Match
        "aa" must Mismatch
        "b" must Match
        "bb" must Mismatch
        "ab" must Match
        "aab" must Mismatch
        "abb" must Mismatch
        "aabb" must Mismatch
        "ba" must Mismatch
      }

      "successfully recognize valid input - `not-predicate` rule sequenced by `charRule` rule" in new TestParser {
        def testRule = rule { !"a" ~ "b" ~ EOI }
        "" must Mismatch
        "a" must Mismatch
        "aa" must Mismatch
        "b" must Match
        "bb" must Mismatch
      }

      "successfully recognize valid input - `oneOrMore` and `seq` combinator rules" in new TestParser {
        def testRule = rule { oneOrMore("a") ~ oneOrMore("b") ~ EOI }
        "" must Mismatch
        "aa" must Mismatch
        "b" must Mismatch
        "bb" must Mismatch
        "ab" must Match
        "aab" must Match
        "abb" must Match
        "aabb" must Match
        "ba" must Mismatch
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