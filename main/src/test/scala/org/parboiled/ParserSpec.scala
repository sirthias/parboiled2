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
    def combination1 = rule { ('a' || 'b' || 'c') ~ ('d' || 'e') ~ 'f' ~ EOI }
  }

  "The new parboiled parser" should {
    "dummy test: check value class instantiation" in {
      println(new TestParser("x").X)
    }

    "successfully recognize single char" in {
      new TestParser("x").X.isMatched must beTrue
      //new TestParser("y").X.isMatched must beFalse
    }

    "successfully recognize valid input - seq combinator rule" in {
      new TestParser("abc").ABC.isMatched must beTrue
      //new TestParser("acb").ABC.isMatched must beFalse
    }

    //    "successfully recognize valid input - firstOf combinator rule" in {
    //      new TestParser("a").ABCfirstOf.isMatched must beTrue
    //      new TestParser("b").ABCfirstOf.isMatched must beTrue
    //      new TestParser("c").ABCfirstOf.isMatched must beTrue
    //      new TestParser("d").ABCfirstOf.isMatched must beFalse
    //    }
    //
    //    "successfully recognize valid input - complex rule" in {
    //      new TestParser("adf").combination1.isMatched must beTrue
    //      new TestParser("bdf").combination1.isMatched must beTrue
    //      new TestParser("aef").combination1.isMatched must beTrue
    //      new TestParser("cef").combination1.isMatched must beTrue
    //      new TestParser("adx").combination1.isMatched must beFalse
    //      new TestParser("bbb").combination1.isMatched must beFalse
    //    }
    //
    //    "properly expand string literals to a sequence of char rules" in {
    //      new TestParser("def").DEF.isMatched must beTrue
    //      new TestParser("dfe").DEF.isMatched must beFalse
    //    }

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