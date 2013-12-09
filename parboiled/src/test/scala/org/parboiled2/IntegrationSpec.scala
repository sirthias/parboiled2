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

class IntegrationSpec extends TestParserSpec {

  "The Parser should correctly recognize/reject input for" >> {

    "(ch('a') | 'b' | 'c') ~ (ch('d') | 'e') ~ 'f' ~ EOI" in new TestParser0 {
      def targetRule = rule { (ch('a') | 'b' | 'c') ~ (ch('d') | 'e') ~ 'f' ~ EOI }
      "adf" must beMatched
      "bdf" must beMatched
      "aef" must beMatched
      "cef" must beMatched
      "adx" must beMismatched
      "bbb" must beMismatched
    }

    "oneOrMore(ANY) ~ EOI" in new TestParser0 {
      def targetRule = rule { oneOrMore(ANY) ~ EOI }
      "a" must beMatched
      "ЖЖ" must beMatched
    }

    """zeroOrMore("a") ~ zeroOrMore("b") ~ EOI""" in new TestParser0 {
      def targetRule = rule { zeroOrMore("a") ~ zeroOrMore("b") ~ EOI }
      "" must beMatched
      "aa" must beMatched
      "b" must beMatched
      "bb" must beMatched
      "ab" must beMatched
      "ba" must beMismatched
    }

    """&("a") ~ "a" ~ EOI""" in new TestParser0 {
      def targetRule = rule { &("a") ~ "a" ~ EOI }
      "" must beMismatched
      "a" must beMatched
      "aa" must beMismatched
      "b" must beMismatched
      "bb" must beMismatched
    }

    """optional("a") ~ optional("b") ~ EOI""" in new TestParser0 {
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

    """!"a" ~ "b" ~ EOI""" in new TestParser0 {
      def targetRule = rule { !"a" ~ "b" ~ EOI }
      "" must beMismatched
      "a" must beMismatched
      "aa" must beMismatched
      "b" must beMatched
      "bb" must beMismatched
    }

    """oneOrMore("a") ~ oneOrMore("b") ~ EOI""" in new TestParser0 {
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
}