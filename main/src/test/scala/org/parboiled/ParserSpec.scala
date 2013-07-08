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
  "The new parboiled parser" should {
    "successfully recognize single char" in {
      case class test(input: ParserInput) extends Parser {
        def X = rule { 'x' ~ EOI }
      }
      test("x").X.matched must beTrue
      test("y").X.matched must beFalse
    }

    "successfully recognize valid input - `seq` combinator rule" in {
      case class test(input: ParserInput) extends Parser {
        def ABC = rule { 'a' ~ 'b' ~ 'c' ~ EOI }
      }
      test("abc").ABC.matched must beTrue
      test("acb").ABC.matched must beFalse
    }

    "successfully recognize valid input - `firstOf` combinator rule" in {
      case class test(input: ParserInput) extends Parser {
        def ABCfirstOf = rule { (ch('a') | 'b' | 'c') ~ EOI }
      }
      test("a").ABCfirstOf.matched must beTrue
      test("b").ABCfirstOf.matched must beTrue
      test("c").ABCfirstOf.matched must beTrue
      test("d").ABCfirstOf.matched must beFalse
    }

    "successfully recognize valid input - `zeroOrMore` combinator rule" in {
      case class test(input: ParserInput) extends Parser {
        def AZeroOrMore = rule { zeroOrMore("a") ~ EOI }
      }
      test("").AZeroOrMore.matched must beTrue
      test("a").AZeroOrMore.matched must beTrue
      test("aa").AZeroOrMore.matched must beTrue
      test("b").AZeroOrMore.matched must beFalse
    }

    "successfully recognize valid input - `oneOrMore` combinator rule" in {
      case class test(input: ParserInput) extends Parser {
        def AOneOrMore = rule { oneOrMore("a") ~ EOI }
      }
      test("").AOneOrMore.matched must beFalse
      test("a").AOneOrMore.matched must beTrue
      test("aa").AOneOrMore.matched must beTrue
      test("b").AOneOrMore.matched must beFalse
    }

    "successfully recognize valid input - `optional` combinator rule" in {
      case class test(input: ParserInput) extends Parser {
        def AOptional = rule { optional("a") ~ EOI }
      }
      test("").AOptional.matched must beTrue
      test("a").AOptional.matched must beTrue
      test("aa").AOptional.matched must beFalse
      test("b").AOptional.matched must beFalse
    }

    "successfully recognize valid input - `not-predicate` combinator rule" in {
      case class test(input: ParserInput) extends Parser {
        def NotA = rule { !"a" ~ EOI }
      }
      test("").NotA.matched must beTrue
      test("a").NotA.matched must beFalse
      test("aa").NotA.matched must beFalse
      test("b").NotA.matched must beFalse
    }

    "successfully recognize valid input - `and-predicate` combinator rule" in {
      case class test(input: ParserInput) extends Parser {
        def AndA = rule { &("a") ~ EOI }
      }
      test("").AndA.matched must beFalse
      test("a").AndA.matched must beFalse
      test("aa").AndA.matched must beFalse
      test("b").AndA.matched must beFalse
    }

    "properly expand string literals to a sequence of char rules" in {
      case class test(input: ParserInput) extends Parser {
        def DEF = rule { "def" }
      }
      test("def").DEF.matched must beTrue
      test("dfe").DEF.matched must beFalse
    }

    "pass integration tests" in {
      "successfully recognize valid input - combination of rules" in {
        case class test(input: ParserInput) extends Parser {
          def combination = rule { (ch('a') | 'b' | 'c') ~ (ch('d') | 'e') ~ 'f' ~ EOI }
        }
        test("adf").combination.matched must beTrue
        test("bdf").combination.matched must beTrue
        test("aef").combination.matched must beTrue
        test("cef").combination.matched must beTrue
        test("adx").combination.matched must beFalse
        test("bbb").combination.matched must beFalse
      }

      "successfully recognize valid input - `zeroOrMore` and `seq` combinator rules" in {
        case class test(input: ParserInput) extends Parser {
          def ABZeroOrMore = rule { zeroOrMore("a") ~ zeroOrMore("b") ~ EOI }
        }
        test("").ABZeroOrMore.matched must beTrue
        test("aa").ABZeroOrMore.matched must beTrue
        test("b").ABZeroOrMore.matched must beTrue
        test("bb").ABZeroOrMore.matched must beTrue
        test("ab").ABZeroOrMore.matched must beTrue
        test("ba").ABZeroOrMore.matched must beFalse
      }

      "successfully recognize valid input - `and-predicate` rule sequenced by `charRule` rule" in {
        case class test(input: ParserInput) extends Parser {
          def AndASeqA = rule { &("a") ~ "a" ~ EOI }
        }
        test("").AndASeqA.matched must beFalse
        test("a").AndASeqA.matched must beTrue
        test("aa").AndASeqA.matched must beFalse
        test("b").AndASeqA.matched must beFalse
        test("bb").AndASeqA.matched must beFalse
      }

      "successfully recognize valid input - `optional` and `seq` combinator rules" in {
        case class test(input: ParserInput) extends Parser {
          def ABOptional = rule { optional("a") ~ optional("b") ~ EOI }
        }
        test("").ABOptional.matched must beTrue
        test("aa").ABOptional.matched must beFalse
        test("b").ABOptional.matched must beTrue
        test("bb").ABOptional.matched must beFalse
        test("ab").ABOptional.matched must beTrue
        test("aab").ABOptional.matched must beFalse
        test("abb").ABOptional.matched must beFalse
        test("aabb").ABOptional.matched must beFalse
        test("ba").ABOptional.matched must beFalse
      }

      "successfully recognize valid input - `not-predicate` rule sequenced by `charRule` rule" in {
        case class test(input: ParserInput) extends Parser {
          def NotASeqB = rule { !"a" ~ "b" ~ EOI }
        }
        test("").NotASeqB.matched must beFalse
        test("a").NotASeqB.matched must beFalse
        test("aa").NotASeqB.matched must beFalse
        test("b").NotASeqB.matched must beTrue
        test("bb").NotASeqB.matched must beFalse
      }

      "successfully recognize valid input - `oneOrMore` and `seq` combinator rules" in {
        case class test(input: ParserInput) extends Parser {
          def ABOneOrMore = rule { oneOrMore("a") ~ oneOrMore("b") ~ EOI }
        }
        test("").ABOneOrMore.matched must beFalse
        test("aa").ABOneOrMore.matched must beFalse
        test("b").ABOneOrMore.matched must beFalse
        test("bb").ABOneOrMore.matched must beFalse
        test("ab").ABOneOrMore.matched must beTrue
        test("aab").ABOneOrMore.matched must beTrue
        test("abb").ABOneOrMore.matched must beTrue
        test("aabb").ABOneOrMore.matched must beTrue
        test("ba").ABOneOrMore.matched must beFalse
      }
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