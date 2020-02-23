/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2

import shapeless.test.illTyped
import utest.{TestableString => _, _}

object BasicSpec extends TestParserSpec {

  val tests = Tests {

    import utest.TestableString

    "The Parser should correctly recognize/reject input for" - {

      "simple char literals" - new TestParser0 {
        def targetRule = rule('x')
        "x" must beMatched
        "y" must beMismatched
        "" must beMismatched
      }

      "a simple char `val`" - new TestParser0 {
        val c          = 'x'
        def targetRule = rule(c)
        "x" must beMatched
        "y" must beMismatched
        "" must beMismatched
      }

      "a simple char `def`" - new TestParser0 {
        def c          = 'x'
        def targetRule = rule(c)
        "x" must beMatched
        "y" must beMismatched
        "" must beMismatched
      }

      "simple string literals" - new TestParser0 {
        def targetRule = rule("ab" ~ EOI)
        "" must beMismatched
        "a" must beMismatched
        "ab" must beMatched
        "abc" must beMismatched
      }

      "a simple string `val`" - new TestParser0 {
        val s          = "ab"
        def targetRule = rule(s ~ EOI)
        "" must beMismatched
        "a" must beMismatched
        "ab" must beMatched
        "abc" must beMismatched
      }

      "a simple string `def`" - new TestParser0 {
        def s          = "ab"
        def targetRule = rule(s ~ EOI)
        "" must beMismatched
        "a" must beMismatched
        "ab" must beMatched
        "abc" must beMismatched
      }

      "a CharPredicate" - new TestParser0 {
        def targetRule = rule(CharPredicate.Digit)
        "0" must beMatched
        "8" must beMatched
        "x" must beMismatched
        "" must beMismatched
      }

      "anyOf" - new TestParser0 {
        def targetRule = rule(anyOf("abc") ~ EOI)
        "" must beMismatched
        "a" must beMatched
        "b" must beMatched
        "c" must beMatched
        "d" must beMismatched
        "ab" must beMismatched
      }

      "noneOf" - new TestParser0 {
        def targetRule = rule(noneOf("abc") ~ EOI)
        "" must beMismatched
        "a" must beMismatched
        "b" must beMismatched
        "c" must beMismatched
        "d" must beMatched
        "ab" must beMismatched
      }

      "ignoreCase(char)" - new TestParser0 {
        def targetRule = rule(ignoreCase('x') ~ EOI)
        "" must beMismatched
        "x" must beMatched
        "X" must beMatched
        "y" must beMismatched
      }

      "ignoreCase(string)" - new TestParser0 {
        def targetRule = rule(ignoreCase("ab") ~ EOI)
        "" must beMismatched
        "a" must beMismatched
        "ab" must beMatched
        "Ab" must beMatched
        "aB" must beMatched
        "abc" must beMismatched
      }

      "ANY" - new TestParser0 {
        def targetRule = rule(ANY)
        "a" must beMatched
        "Ж" must beMatched
        "" must beMismatched
      }

      "EOI" - new TestParser0 {
        def targetRule = rule(EOI)
        "" must beMatched
        "x" must beMismatched
      }

      "character ranges" - new TestParser0 {
        // shadow utests implicit extension on Strings which collides with our `str2CharRangeSupport`
        override def TestableString = rule(("1" - "5") ~ EOI)
        def targetRule              = TestableString

        "1" must beMatched
        "3" must beMatched
        "5" must beMatched
        "" must beMismatched
        "0" must beMismatched
        "a" must beMismatched
        "8" must beMismatched
      }

      "MATCH" - new TestParser0 {
        def targetRule = rule(MATCH ~ EOI)
        "" must beMatched
        "x" must beMismatched
      }

      "called rules" - new TestParser0 {
        def targetRule = {
          def free() = rule("-free")
          rule(foo ~ bar(42) ~ baz("", 1337) ~ typed[String] ~ free() ~ EOI)
        }
        def foo                    = rule("foo")
        def bar(i: Int)            = rule("-bar" ~ i.toString)
        def baz(s: String, i: Int) = rule("-baz" ~ s ~ i.toString)
        def typed[S <: String]     = rule(MATCH)
        "foo-bar42-baz1337-free" must beMatched
      }

      "Map[String, T]" - new TestParser1[Int] {
        val colors     = Map("red" -> 1, "green" -> 2, "blue" -> 3)
        def targetRule = rule(colors ~ EOI)
        "red" must beMatchedWith(1)
        "green" must beMatchedWith(2)
        "blue" must beMatchedWith(3)
        "black" must beMismatched
      }

      "Map[String, T] that ignores case" - new TestParser1[Int] {
        val colors     = Map("red" -> 1, "green" -> 2, "blue" -> 3)
        def targetRule = rule(valueMap(colors, ignoreCase = true) ~ EOI)
        "ReD" must beMatchedWith(1)
        "grEen" must beMatchedWith(2)
        "blUe" must beMatchedWith(3)
        "black" must beMismatched
      }

      "Map[String, T] with keys that prefix each other" - new TestParser1[Int] {
        val map        = Map("a" -> 1, "ab" -> 2, "abc" -> 3, "abcd" -> 4, "abcde" -> 5, "abcdef" -> 6)
        def targetRule = rule(map ~ EOI)
        "a" must beMatchedWith(1)
        "ab" must beMatchedWith(2)
        "abc" must beMatchedWith(3)
        "abcd" must beMatchedWith(4)
        "abcde" must beMatchedWith(5)
        "abcdef" must beMatchedWith(6)
      }
    }

    "The Parser" - {
      "disallow compilation of an illegal character range" - new Parser {
        // shadow utests implicit extension on Strings which collides with our `str2CharRangeSupport`
        def TestableString = ParserInput.Empty
        def input          = TestableString

        illTyped("""rule { "00" - "5" }""", "lower bound must be a single char string")
        illTyped("""rule { "0" - "55" }""", "upper bound must be a single char string")
        illTyped("""rule { "" - "5" }""", "lower bound must be a single char string")
        illTyped("""rule { "0" - "" }""", "upper bound must be a single char string")
        illTyped("""rule { "5" - "1" }""", "lower bound must not be > upper bound")
      }
    }
  }
}
