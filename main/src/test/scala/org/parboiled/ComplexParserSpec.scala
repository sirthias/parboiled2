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

class ComplexParserSpec extends Specification {
  class TestParser(val input: ParserInput) extends Parser {
    def ABC = rule { 'a' ~ 'b' ~ 'c' }
    def complexRule = rule { ABC ~ ABC ~ EOI }
  }

  "A complex parboiled parser" should {
    "successfully recognize complex rule" in {
      new TestParser("abcabc").complexRule.matched must beTrue
      new TestParser("abcbc").complexRule.matched must beFalse
      new TestParser("abcbc").complexRule.matched must beFalse
      new TestParser("abc").complexRule.matched must beFalse
      new TestParser("y").complexRule.matched must beFalse
    }
  }
}