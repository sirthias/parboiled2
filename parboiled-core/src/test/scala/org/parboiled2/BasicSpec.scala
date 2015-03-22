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

import shapeless.test.illTyped
import org.specs2.specification.Scope

class BasicSpec extends TestParserSpec {

  "The Parser should correctly recognize/reject input for" >> {

    "simple char literals" in new TestParser0 {
      val targetRule = rule { 'x' }
      "x" must beMatched
      "y" must beMismatched
      "" must beMismatched
    }

    "a simple char `val`" in new TestParser0 {
      val c = 'x'
      val targetRule = rule { c }
      "x" must beMatched
      "y" must beMismatched
      "" must beMismatched
    }

    "a simple char `def`" in new TestParser0 {
      def c = 'x'
      val targetRule = rule { c }
      "x" must beMatched
      "y" must beMismatched
      "" must beMismatched
    }

    "simple string literals" in new TestParser0 {
      val targetRule = rule { "ab" ~ EOI }
      "" must beMismatched
      "a" must beMismatched
      "ab" must beMatched
      "abc" must beMismatched
    }

    "a simple string `val`" in new TestParser0 {
      val s = "ab"
      val targetRule = rule { s ~ EOI }
      "" must beMismatched
      "a" must beMismatched
      "ab" must beMatched
      "abc" must beMismatched
    }

    "a simple string `def`" in new TestParser0 {
      def s = "ab"
      val targetRule = rule { s ~ EOI }
      "" must beMismatched
      "a" must beMismatched
      "ab" must beMatched
      "abc" must beMismatched
    }

    "a CharPredicate" in new TestParser0 {
      val targetRule = rule { CharPredicate.Digit }
      "0" must beMatched
      "8" must beMatched
      "x" must beMismatched
      "" must beMismatched
    }

    "anyOf" in new TestParser0 {
      val targetRule = rule { anyOf("abc") ~ EOI }
      "" must beMismatched
      "a" must beMatched
      "b" must beMatched
      "c" must beMatched
      "d" must beMismatched
      "ab" must beMismatched
    }

    "noneOf" in new TestParser0 {
      val targetRule = rule { noneOf("abc") ~ EOI }
      "" must beMismatched
      "a" must beMismatched
      "b" must beMismatched
      "c" must beMismatched
      "d" must beMatched
      "ab" must beMismatched
    }

    "ignoreCase(char)" in new TestParser0 {
      val targetRule = rule { ignoreCase('x') ~ EOI }
      "" must beMismatched
      "x" must beMatched
      "X" must beMatched
      "y" must beMismatched
    }

    "ignoreCase(string)" in new TestParser0 {
      val targetRule = rule { ignoreCase("ab") ~ EOI }
      "" must beMismatched
      "a" must beMismatched
      "ab" must beMatched
      "Ab" must beMatched
      "aB" must beMatched
      "abc" must beMismatched
    }

    "ANY" in new TestParser0 {
      val targetRule = rule { ANY }
      "a" must beMatched
      "Ж" must beMatched
      "" must beMismatched
    }

    "EOI" in new TestParser0 {
      val targetRule = rule { EOI }
      "" must beMatched
      "x" must beMismatched
    }

    "character ranges" in new TestParser0 {
      val targetRule = rule { ("1" - "5") ~ EOI }
      "1" must beMatched
      "3" must beMatched
      "5" must beMatched
      "" must beMismatched
      "0" must beMismatched
      "a" must beMismatched
      "8" must beMismatched
    }

    "MATCH" in new TestParser0 {
      val targetRule = rule { MATCH ~ EOI }
      "" must beMatched
      "x" must beMismatched
    }

    "rule calls to `val`s" in new TestParser0 {
      val targetRule = rule { (val0 | val1(1) | val2(2, '2') | val3(3, "s", 'x')) ~ EOI }
      val val0 = rule { '0' }
      val val1 = rule[Int]() { _ times '1' }
      val val2 = rule[Int, Char]() { _ times _ }
      val val3 = rule[Int, String, Char]() { (i, s, c) ⇒ i times (s + c) }
      "0" must beMatched
      "1" must beMatched
      "22" must beMatched
      "sxsxsx" must beMatched
    }

    "rule calls to `def`s with inner parameters" in new TestParser0 {
      val targetRule = rule { (def0 | def1(1) | def2(2, '2') | def3(3, "s", 'x')) ~ EOI }
      def def0 = rule { '0' }
      def def1 = rule[Int]() { _ times '1' }
      def def2 = rule[Int, Char]() { _ times _ }
      def def3 = rule[Int, String, Char]() { (i, s, c) ⇒ i times (s + c) }
      "0" must beMatched
      "1" must beMatched
      "22" must beMatched
      "sxsxsx" must beMatched
    }

    "rule calls to `def`s with outer parameters" in new TestParser0 {
      val targetRule = rule { (def0 | def1(1) | def2(2, '2') | def3(3, "s", 'x')) ~ EOI }
      def def0 = rule { '0' }
      def def1(i: Int) = rule { i times '1' }
      def def2(i: Int, c: Char) = rule { i times c }
      def def3(i: Int, s: String, c: Char) = rule { i times (s + c) }
      "0" must beMatched
      "1" must beMatched
      "22" must beMatched
      "sxsxsx" must beMatched
    }

    "Map[String, T]" in new TestParser1[Int] {
      val colors = Map("red" -> 1, "green" -> 2, "blue" -> 3)
      val targetRule = rule { colors ~ EOI }
      "red" must beMatchedWith(1)
      "green" must beMatchedWith(2)
      "blue" must beMatchedWith(3)
      "black" must beMismatched
    }
  }

  "The Parser" should {
    "disallow compilation of an illegal character range" in new SimpleParser with Scope {
      illTyped("""rule { "00" - "5" }""", "lower bound must be a single char string")
      illTyped("""rule { "0" - "55" }""", "upper bound must be a single char string")
      illTyped("""rule { "" - "5" }""", "lower bound must be a single char string")
      illTyped("""rule { "0" - "" }""", "upper bound must be a single char string")
      illTyped("""rule { "5" - "1" }""", "lower bound must not be > upper bound")
      success
    }
  }
}
