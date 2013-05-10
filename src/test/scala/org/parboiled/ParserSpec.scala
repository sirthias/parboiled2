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
  class TestParser(_input: ParserInput) extends Parser(_input) {
    def X = rule { 'x' ~ EOI }
    def ABC = rule { 'a' ~ 'b' ~ 'c' ~ EOI }
    def ABCfirstOf = rule { ('a' || 'b' || 'c') ~ EOI }
    def DEF = rule { "def" }
    def combination1 = rule { ('a' || 'b' || 'c') ~ ('d' || 'e') ~ 'f' ~ EOI }
  }

  "The new parboiled parser" should {
    "successfully recognize single char" in {
      new TestParser("x").X must beTrue
      new TestParser("y").X must beFalse
    }

    "successfully recognize valid input - seq combinator rule" in {
      new TestParser("abc").ABC must beTrue
      new TestParser("acb").ABC must beFalse
    }

    "successfully recognize valid input - firstOf combinator rule" in {
      new TestParser("a").ABCfirstOf must beTrue
      new TestParser("b").ABCfirstOf must beTrue
      new TestParser("c").ABCfirstOf must beTrue
      new TestParser("d").ABCfirstOf must beFalse
    }

    "successfully recognize valid input - complex rule" in {
      new TestParser("adf").combination1 must beTrue
      new TestParser("bdf").combination1 must beTrue
      new TestParser("aef").combination1 must beTrue
      new TestParser("cef").combination1 must beTrue
      new TestParser("adx").combination1 must beFalse
      new TestParser("bbb").combination1 must beFalse
    }

    "properly expand string literals to a sequence of char rules" in {
      new TestParser("def").DEF must beTrue
      new TestParser("dfe").DEF must beFalse
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