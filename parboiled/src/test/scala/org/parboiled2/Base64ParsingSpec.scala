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

import scala.util.Random
import org.parboiled2.util.Base64
import shapeless._
import utest._

object Base64ParsingSpec extends TestSuite {

  class TestParser(val input: ParserInput) extends Parser with Base64Parsing

  val randomChars = {
    val random = new Random()
    Stream.continually(random.nextPrintableChar())
  }

  val tests = Tests{

    "Base64Parsing" - {
      "enable parsing of RFC2045 Strings" - test("rfc2045String", Base64.rfc2045())
      "enable parsing of RFC2045 Blocks" - test("rfc2045Block", Base64.rfc2045())
      "enable parsing of custom-Base64 Strings" - test("base64CustomString", Base64.rfc2045())
      "enable parsing of custom-Base64 Blocks" - test("base64CustomBlock", Base64.rfc2045())
    }

  }

  val (dispatch, rules) = DynamicRuleDispatch[TestParser, Array[Byte] :: HNil](
    "rfc2045String", "rfc2045Block", "base64CustomString", "base64CustomBlock")

  def test(ruleName: String, base64: Base64): Unit =
    (1 to 100).foreach { x =>
      val string = randomChars.take(x).toString()
      val encoded = base64.encodeToString(string getBytes UTF8, lineSep = false)
      val parser = new TestParser(encoded) with DynamicRuleHandler[TestParser, Array[Byte]:: HNil] {
        type Result = String
        def parser: TestParser = this
        def ruleNotFound(ruleName: String): Result = "n/a"
        def success(result: Array[Byte] :: HNil): Result = new String(result.head, UTF8)
        def parseError(error: ParseError): Result = sys.error("unexpected parse error")
        def failure(error: Throwable): Result = sys.error("unexpected parser exception")
      }
      assert(dispatch(parser, ruleName) == string)
    }
}
