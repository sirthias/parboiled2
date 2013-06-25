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

class ParserSpec extends Specification {
  class TestParser(val input: ParserInput) extends Parser {
    def X = rule { 'x' ~ EOI }
    def ABC = rule { 'a' ~ 'b' ~ 'c' ~ EOI }
    def ABCfirstOf = rule { ('a' || 'b' || 'c') ~ EOI }
    def DEF = rule { "def" }
    def combination = rule { ('a' || 'b' || 'c') ~ ('d' || 'e') ~ 'f' ~ EOI }
    def AZeroOrMore = rule { zeroOrMore("a") ~ EOI }
    def ABZeroOrMore = rule { zeroOrMore("a") ~ zeroOrMore("b") ~ EOI }
    def AOneOrMore = rule { oneOrMore("a") ~ EOI }
    def ABOneOrMore = rule { oneOrMore("a") ~ oneOrMore("b") ~ EOI }
    def AOptional = rule { optional("a") ~ EOI }
    def ABOptional = rule { optional("a") ~ optional("b") ~ EOI }
  }

  "The new parboiled parser" should {
    "successfully recognize single char" in {
      new TestParser("x").X.matched must beTrue
      new TestParser("y").X.matched must beFalse
    }

    "successfully recognize valid input - seq combinator rule" in {
      new TestParser("abc").ABC.matched must beTrue
      new TestParser("acb").ABC.matched must beFalse
    }

    "successfully recognize valid input - firstOf combinator rule" in {
      new TestParser("a").ABCfirstOf.matched must beTrue
      new TestParser("b").ABCfirstOf.matched must beTrue
      new TestParser("c").ABCfirstOf.matched must beTrue
      new TestParser("d").ABCfirstOf.matched must beFalse
    }

    "successfully recognize valid input - zeroOrMore combinator rule" in {
      new TestParser("").AZeroOrMore.matched must beTrue
      new TestParser("a").AZeroOrMore.matched must beTrue
      new TestParser("aa").AZeroOrMore.matched must beTrue
      new TestParser("b").AZeroOrMore.matched must beFalse
    }

    "successfully recognize valid input - zeroOrMore and seq combinator rules" in {
      new TestParser("").ABZeroOrMore.matched must beTrue
      new TestParser("aa").ABZeroOrMore.matched must beTrue
      new TestParser("b").ABZeroOrMore.matched must beTrue
      new TestParser("bb").ABZeroOrMore.matched must beTrue
      new TestParser("ab").ABZeroOrMore.matched must beTrue
      new TestParser("ba").ABZeroOrMore.matched must beFalse
    }

    "successfully recognize valid input - oneOrMore combinator rule" in {
      new TestParser("").AOneOrMore.matched must beFalse
      new TestParser("a").AOneOrMore.matched must beTrue
      new TestParser("aa").AOneOrMore.matched must beTrue
      new TestParser("b").AOneOrMore.matched must beFalse
    }

    // TODO: Move to integration tests
    "successfully recognize valid input - oneOrMore and seq combinator rules" in {
      new TestParser("").ABOneOrMore.matched must beFalse
      new TestParser("aa").ABOneOrMore.matched must beFalse
      new TestParser("b").ABOneOrMore.matched must beFalse
      new TestParser("bb").ABOneOrMore.matched must beFalse
      new TestParser("ab").ABOneOrMore.matched must beTrue
      new TestParser("aab").ABOneOrMore.matched must beTrue
      new TestParser("abb").ABOneOrMore.matched must beTrue
      new TestParser("aabb").ABOneOrMore.matched must beTrue
      new TestParser("ba").ABOneOrMore.matched must beFalse
    }

    "successfully recognize valid input - optional combinator rule" in {
      new TestParser("").AOptional.matched must beTrue
      new TestParser("a").AOptional.matched must beTrue
      new TestParser("aa").AOptional.matched must beFalse
      new TestParser("b").AOptional.matched must beFalse
    }

    // TODO: Move to integration tests
    "successfully recognize valid input - optional and seq combinator rules" in {
      new TestParser("").ABOptional.matched must beTrue
      new TestParser("aa").ABOptional.matched must beFalse
      new TestParser("b").ABOptional.matched must beTrue
      new TestParser("bb").ABOptional.matched must beFalse
      new TestParser("ab").ABOptional.matched must beTrue
      new TestParser("aab").ABOptional.matched must beFalse
      new TestParser("abb").ABOptional.matched must beFalse
      new TestParser("aabb").ABOptional.matched must beFalse
      new TestParser("ba").ABOptional.matched must beFalse
    }

    "successfully recognize valid input - combination rule" in {
      new TestParser("adf").combination.matched must beTrue
      new TestParser("bdf").combination.matched must beTrue
      new TestParser("aef").combination.matched must beTrue
      new TestParser("cef").combination.matched must beTrue
      new TestParser("adx").combination.matched must beFalse
      new TestParser("bbb").combination.matched must beFalse
    }

    "properly expand string literals to a sequence of char rules" in {
      new TestParser("def").DEF.matched must beTrue
      new TestParser("dfe").DEF.matched must beFalse
    }

    // TODO: Fix this test
    //    "disallow compilation of an illegal string rule" in {
    //      CompilerError.verify(
    //        """class TestParser(_input: ParserInput) extends Parser(_input) {
    //                       val string = "def"
    //                       def Illegal = rule { string } // not allowed, strings must be given as literals
    //                     }""",
    //        "Strings in rule definitions have to be literals")
  }
}