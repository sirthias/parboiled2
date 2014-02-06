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

import org.specs2.mutable.Specification
import shapeless.HNil
import scala.util.Success

class UtilsSpec extends Specification {

  class TestParser(val input: ParserInput) extends Parser {
    def A = rule('a' ~ EOI)
    def B = rule('b' ~ EOI)
    def C = rule('c' ~ EOI)
    def D = rule('d' ~ EOI)
    def E = rule('e' ~ EOI)
  }

  "Utils.createDynamicRuleDispatch" should {
    "work as expected when selecting from 0 rules" in {
      val dispatch = DynamicRuleDispatch[TestParser, HNil]()
      dispatch(new TestParser("a"), "A") === None
    }
    "work as expected when selecting from 1 rule" in {
      val dispatch = DynamicRuleDispatch[TestParser, HNil]("A")
      dispatch(new TestParser("a"), "A").get.run() === Success(())
      dispatch(new TestParser("b"), "B") === None
    }
    "work as expected when selecting from 2 rules" in {
      val dispatch = DynamicRuleDispatch[TestParser, HNil]("A", "B")
      dispatch(new TestParser("a"), "A").get.run() === Success(())
      dispatch(new TestParser("b"), "B").get.run() === Success(())
      dispatch(new TestParser("c"), "C") === None
    }
    "work as expected when selecting from 3 rules" in {
      val dispatch = DynamicRuleDispatch[TestParser, HNil]("A", "B", "C")
      dispatch(new TestParser("a"), "A").get.run() === Success(())
      dispatch(new TestParser("b"), "B").get.run() === Success(())
      dispatch(new TestParser("c"), "C").get.run() === Success(())
      dispatch(new TestParser("d"), "D") === None
    }
    "work as expected when selecting from 4 rules" in {
      val dispatch = DynamicRuleDispatch[TestParser, HNil]("A", "B", "C", "D")
      dispatch(new TestParser("a"), "A").get.run() === Success(())
      dispatch(new TestParser("b"), "B").get.run() === Success(())
      dispatch(new TestParser("c"), "C").get.run() === Success(())
      dispatch(new TestParser("d"), "D").get.run() === Success(())
      dispatch(new TestParser("e"), "E") === None
    }
    "work as expected when selecting from 5 rules" in {
      val dispatch = DynamicRuleDispatch[TestParser, HNil]("A", "B", "C", "D", "E")
      dispatch(new TestParser("a"), "A").get.run() === Success(())
      dispatch(new TestParser("b"), "B").get.run() === Success(())
      dispatch(new TestParser("c"), "C").get.run() === Success(())
      dispatch(new TestParser("d"), "D").get.run() === Success(())
      dispatch(new TestParser("e"), "E").get.run() === Success(())
      dispatch(new TestParser("f"), "F") === None
    }
  }
}
