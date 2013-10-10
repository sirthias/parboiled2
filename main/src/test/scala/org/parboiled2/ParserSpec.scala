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

class ParserSpec extends TestParserSpec {
  "The new parboiled parser" should {
    "successfully recognize single char" in new TestParser0 {
      def targetRule = rule { 'x' }
      "x" must beMatched
      "y" must beMismatched
    }

    "successfully recognize string provided by `val`" in new TestParser0 {
      val aString = "a"
      def targetRule = rule { aString }

      "a" must beMatched
      "b" must beMismatched
    }

    "successfully recognize string provided by `def`" in new TestParser0 {
      def aString = "a"
      def targetRule = rule { aString }

      "a" must beMatched
      "b" must beMismatched
    }

    "successfully recognize valid input - `seq` combinator rule" in new TestParser0 {
      def targetRule = rule { 'a' ~ 'b' }
      "ab" must beMatched
      "ac" must beMismatched
      "bb" must beMismatched
    }

    "successfully recognize valid input - `firstOf` combinator rule" in new TestParser0 {
      def targetRule = rule { ch('a') | 'b' }
      "a" must beMatched
      "b" must beMatched
      "c" must beMismatched
    }

    "successfully recognize valid input - `zeroOrMore` combinator rule" in new TestParser0 {
      def targetRule = rule { zeroOrMore("a") }
      "a" must beMatched
      "aa" must beMatched
      "b" must beMatched
    }

    "successfully recognize valid input - `oneOrMore` combinator rule" in new TestParser0 {
      def targetRule = rule { oneOrMore("a") }
      "a" must beMatched
      "aa" must beMatched
      "b" must beMismatched
    }

    "successfully recognize valid input - `optional` combinator rule" in new TestParser0 {
      def targetRule = rule { optional("a") }
      "" must beMatched
      "a" must beMatched
      "b" must beMatched
    }

    "successfully recognize valid input - `not-predicate` combinator rule" in new TestParser0 {
      def targetRule = rule { !"a" }
      "" must beMatched
      "a" must beMismatched
      "aa" must beMismatched
      "b" must beMatched
    }

    "successfully recognize valid input - `and-predicate` combinator rule" in new TestParser0 {
      def targetRule = rule { &("a") }
      "a" must beMatched
      "aa" must beMatched
      "b" must beMismatched
    }

    "successfully recognize valid input - `character-class` rule" in new TestParser0 {
      def targetRule = rule { "1" - "5" }
      "1" must beMatched
      "3" must beMatched
      "5" must beMatched
      "" must beMismatched
      "0" must beMismatched
      "a" must beMismatched
      "8" must beMismatched
    }

    "successfully recognize EOI" in new TestParser0 {
      def targetRule = rule { EOI }
      "" must beMatched
      "x" must beMismatched
    }

    "successfully recognize ANY" in new TestParser0 {
      def targetRule = rule { ANY }
      "a" must beMatched
      "Ж" must beMatched
      "" must beMismatched
    }

    "successfully recognize EMPTY" in new TestParser0 {
      def targetRule = rule { "a" ~ EMPTY ~ EOI }
      "a" must beMatched
      "b" must beMismatched
    }

    "successfully recognize `test`" in new TestParser0 {
      var flag = true
      def targetRule = rule { "a" ~ test(flag) ~ EOI }

      "a" must beMatched

      flag = false
      "a" must beMismatched
    }

    "successfully work with `nTimes` with zero times" in new TestParser0 {
      def targetRule = rule { "a" ~ nTimes(0, "a") ~ EOI }

      "a" must beMatched
      "aa" must beMismatched
    }

    "successfully work with `nTimes` with default empty separator" in new TestParser0 {
      def targetRule = rule { nTimes(2, "a") ~ EOI }

      "a" must beMismatched
      "aa" must beMatched
    }

    "successfully work with `nTimes` with custom separator" in new TestParser0 {
      def targetRule = rule { nTimes(2, "a", "|") ~ EOI }

      "a" must beMismatched
      "a|a" must beMatched
    }

    "properly expand string literals to a sequence of char rules" in new TestParser0 {
      def targetRule = rule { "def" }
      "def" must beMatched
      "dfe" must beMismatched
    }

    "pass integration tests" in {
      "successfully recognize valid input - combination of rules" in new TestParser0 {
        def targetRule = rule { (ch('a') | 'b' | 'c') ~ (ch('d') | 'e') ~ 'f' ~ EOI }
        "adf" must beMatched
        "bdf" must beMatched
        "aef" must beMatched
        "cef" must beMatched
        "adx" must beMismatched
        "bbb" must beMismatched
      }

      "successfully recognize valid input - `oneOrMore` of `ANY` combinator rules" in new TestParser0 {
        def targetRule = rule { oneOrMore(ANY) ~ EOI }

        "a" must beMatched
        "ЖЖ" must beMatched
      }

      "successfully recognize valid input - `zeroOrMore` and `seq` combinator rules" in new TestParser0 {
        def targetRule = rule { zeroOrMore("a") ~ zeroOrMore("b") ~ EOI }
        "" must beMatched
        "aa" must beMatched
        "b" must beMatched
        "bb" must beMatched
        "ab" must beMatched
        "ba" must beMismatched
      }

      "successfully recognize valid input - `and-predicate` rule sequenced by `charRule` rule" in new TestParser0 {
        def targetRule = rule { &("a") ~ "a" ~ EOI }
        "" must beMismatched
        "a" must beMatched
        "aa" must beMismatched
        "b" must beMismatched
        "bb" must beMismatched
      }

      "successfully recognize valid input - `optional` and `seq` combinator rules" in new TestParser0 {
        def targetRule = rule { optional("a") ~ optional("b") ~ EOI }
        "" must beMatched
        "aa" must beMismatched
        "b" must beMatched
        "bb" must beMismatched
        "ab" must beMatched
        "aab" must beMismatched
        "abb" must beMismatched
        "aabb" must beMismatched
        "ba" must beMismatched
      }

      "successfully recognize valid input - `not-predicate` rule sequenced by `charRule` rule" in new TestParser0 {
        def targetRule = rule { !"a" ~ "b" ~ EOI }
        "" must beMismatched
        "a" must beMismatched
        "aa" must beMismatched
        "b" must beMatched
        "bb" must beMismatched
      }

      "successfully recognize valid input - `oneOrMore` and `seq` combinator rules" in new TestParser0 {
        def targetRule = rule { oneOrMore("a") ~ oneOrMore("b") ~ EOI }
        "" must beMismatched
        "aa" must beMismatched
        "b" must beMismatched
        "bb" must beMismatched
        "ab" must beMatched
        "aab" must beMatched
        "abb" must beMatched
        "aabb" must beMatched
        "ba" must beMismatched
      }
    }

    // TODO: Fix this test
    // "disallow compilation of an illegal `character-class` rule" in new TestParser0 {
    //   def targetRule = rule { "00" - "5" }
    //   def targetRule = rule { "0" - "55" }
    //   def targetRule = rule { "" - "5" }
    //   def targetRule = rule { "0" - "" }
    //   def targetRule = rule { "5" - "1" }
    //   def startDigit = rule { "1" }
    //   def targetRule = rule { startDigit - "9" }
    // }

    // TODO: Fix this test
    //    "disallow compilation of an illegal string rule" in new TestParser0 {
    //      CompilerError.verify(
    //        """class TestParser0(_input: ParserInput) extends Parser(_input) {
    //                       val string = "def"
    //                       def Illegal = rule { string } // not allowed, strings must be given as literals
    //                     }""",
    //        "Strings in rule definitions have to be literals")
  }
}