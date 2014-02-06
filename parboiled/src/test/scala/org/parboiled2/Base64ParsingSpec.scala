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
import scala.util.{ Random, Success }
import org.parboiled2.util.Base64
import shapeless._

class Base64ParsingSpec extends Specification {

  class TestParser(val input: ParserInput) extends Parser with Base64Parsing

  val randomChars = {
    val random = new Random()
    Stream.continually(random.nextPrintableChar())
  }

  "Base64Parsing" should {
    "enable parsing of RFC2045 Strings" in test("rfc2045String", Base64.rfc2045())
    "enable parsing of RFC2045 Blocks" in test("rfc2045Block", Base64.rfc2045())
    "enable parsing of custom-Base64 Strings" in test("base64CustomString", Base64.rfc2045())
    "enable parsing of custom-Base64 Blocks" in test("base64CustomBlock", Base64.rfc2045())
  }

  val dispatch = DynamicRuleDispatch[TestParser, Array[Byte] :: HNil](
    "rfc2045String", "rfc2045Block", "base64CustomString", "base64CustomBlock")

  def test(ruleName: String, base64: Base64) =
    (1 to 100).map { x ⇒
      val string = randomChars.take(x).toString()
      val encoded = base64.encodeToString(string getBytes UTF8, false)
      dispatch(new TestParser(encoded), ruleName).get.run().map(new String(_, UTF8)) === Success(string)
    }.reduceLeft(_ and _)
}
