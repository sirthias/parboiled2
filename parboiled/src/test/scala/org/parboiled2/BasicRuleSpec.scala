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

class BasicRuleSpec extends TestParserSpec {

  "The Parser should correctly recognize/reject input for" >> {

    "single char literal" in new TestParser0 {
      def targetRule = rule { 'x' }
      "x" must beMatched
      "y" must beMismatched
      "" must beMismatched
    }

    "single char `val`" in new TestParser0 {
      val c = "x"
      def targetRule = rule { c }

      "x" must beMatched
      "y" must beMismatched
      "" must beMismatched
    }

    "single char `def`" in new TestParser0 {
      def c = "x"
      def targetRule = rule { c }

      "x" must beMatched
      "y" must beMismatched
      "" must beMismatched
    }

    "simple strings" in new TestParser0 {
      def targetRule = rule { str("abc") ~ EOI }
      "" must beMismatched
      "a" must beMismatched
      "ab" must beMismatched
      "abc" must beMatched
      "abcd" must beMismatched
    }

    "character ranges" in new TestParser0 {
      def targetRule = rule { ("1" - "5") ~ EOI }
      "1" must beMatched
      "3" must beMatched
      "5" must beMatched
      "" must beMismatched
      "0" must beMismatched
      "a" must beMismatched
      "8" must beMismatched
    }

    "EOI" in new TestParser0 {
      def targetRule = rule { EOI }
      "" must beMatched
      "x" must beMismatched
    }

    "ANY" in new TestParser0 {
      def targetRule = rule { ANY }
      "a" must beMatched
      "Ж" must beMatched
      "" must beMismatched
    }

    "EMPTY" in new TestParser0 {
      def targetRule = rule { EMPTY ~ EOI }
      "" must beMatched
      "x" must beMismatched
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