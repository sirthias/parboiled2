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

import shapeless.HNil
import utest._

object DynamicRuleDispatchSpec extends TestSuite {

  class TestParser(val input: ParserInput) extends Parser with DynamicRuleHandler[TestParser, HNil] {
    def A = rule('a' ~ EOI)
    def B = rule('b' ~ EOI)
    def C = rule('c' ~ EOI)
    def D = rule('d' ~ EOI)
    def E = rule('e' ~ EOI)

    type Result = String
    def parser: TestParser = this
    def ruleNotFound(ruleName: String): Result = "n/a"
    def success(result: HNil): Result = "ok"
    def parseError(error: ParseError): Result = "error"
    def failure(error: Throwable): Result = "throw"
  }

  val tests = Tests{

    "DynamicRuleDispatch" - {
      "work as expected when selecting from 0 rules" - {
        val (dispatch, ruleNames) = DynamicRuleDispatch[TestParser, HNil]()
        dispatch(new TestParser("a"), "A") ==> "n/a"
        ruleNames ==> Seq()
      }
      "work as expected when selecting from 1 rule" - {
        val (dispatch, ruleNames) = DynamicRuleDispatch[TestParser, HNil]("A")
        dispatch(new TestParser("a"), "A") ==> "ok"
        dispatch(new TestParser("b"), "B") ==> "n/a"
        ruleNames ==> Seq("A")
      }
      "work as expected when selecting from 2 rules" - {
        val (dispatch, ruleNames) = DynamicRuleDispatch[TestParser, HNil]("A", "B")
        dispatch(new TestParser("a"), "A") ==> "ok"
        dispatch(new TestParser("b"), "B") ==> "ok"
        dispatch(new TestParser("c"), "C") ==> "n/a"
        ruleNames ==> Seq("A", "B")
      }
      "work as expected when selecting from 3 rules" - {
        val (dispatch, ruleNames) = DynamicRuleDispatch[TestParser, HNil]("A", "B", "C")
        dispatch(new TestParser("a"), "A") ==> "ok"
        dispatch(new TestParser("b"), "B") ==> "ok"
        dispatch(new TestParser("c"), "C") ==> "ok"
        dispatch(new TestParser("d"), "D") ==> "n/a"
        ruleNames ==> Seq("A", "B", "C")
      }
      "work as expected when selecting from 4 rules" - {
        val (dispatch, ruleNames) = DynamicRuleDispatch[TestParser, HNil]("A", "B", "C", "D")
        dispatch(new TestParser("a"), "A") ==> "ok"
        dispatch(new TestParser("b"), "B") ==> "ok"
        dispatch(new TestParser("c"), "C") ==> "ok"
        dispatch(new TestParser("d"), "D") ==> "ok"
        dispatch(new TestParser("e"), "E") ==> "n/a"
        ruleNames ==> Seq("A", "B", "C", "D")
      }
      "work as expected when selecting from 5 rules" - {
        val (dispatch, ruleNames) = DynamicRuleDispatch[TestParser, HNil]("A", "B", "C", "D", "E")
        dispatch(new TestParser("a"), "A") ==> "ok"
        dispatch(new TestParser("b"), "B") ==> "ok"
        dispatch(new TestParser("c"), "C") ==> "ok"
        dispatch(new TestParser("d"), "D") ==> "ok"
        dispatch(new TestParser("e"), "E") ==> "ok"
        dispatch(new TestParser("f"), "F") ==> "n/a"
        ruleNames ==> Seq("A", "B", "C", "D", "E")
      }
    }
  }
}
