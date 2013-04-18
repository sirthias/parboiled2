/*
 * Copyright (C) 2009-2013 Mathias Doenitz
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
    def ABC = rule { 'a' ~ 'b' ~ 'c' }
    def DEF = rule { "def" }
  }

  "The new parboiled parser" should {

    "successfully recognize valid input" in {
      new TestParser("abc").ABC must beTrue
    }

    "successfully recognize valid input" in {
      new TestParser("acb").ABC must beFalse
    }

    "properly expand string literals to a sequence of char rules" in {
      new TestParser("def").DEF must beTrue
      new TestParser("dfe").DEF must beFalse
    }

    "disallow compilation of an illegal string rule" in {
      CompilerError.verify(
        """class TestParser(_input: ParserInput) extends Parser(_input) {
             val string = "def"
             def Illegal = rule { string } // not allowed, strings must be given as literals
           }""",
        "Strings in rule definitions have to be literals")
    }
  }

}
