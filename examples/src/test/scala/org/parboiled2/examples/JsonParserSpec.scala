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

package org.parboiled2.examples

import scala.util.{Failure, Success}
import org.specs2.mutable.Specification
import spray.json.{JsonParser => _, _}
import org.parboiled2._

class JsonParserSpec extends Specification {

  "The JsonParser" should {
    "parse 'null' to JsNull" in {
      parse("null") === JsNull
    }
    "parse 'true' to JsTrue" in {
      parse("true") === JsTrue
    }
    "parse 'false' to JsFalse" in {
      parse("false") === JsFalse
    }
    "parse '0' to JsNumber" in {
      parse("0") === JsNumber(0)
    }
    "parse '1.23' to JsNumber" in {
      parse("1.23") === JsNumber(1.23)
    }
    "parse '-1E10' to JsNumber" in {
      parse("-1E10") === JsNumber("-1E+10")
    }
    "parse '12.34e-10' to JsNumber" in {
      parse("12.34e-10") === JsNumber("1.234E-9")
    }
    "parse \"xyz\" to JsString" in {
      parse("\"xyz\"") === JsString("xyz")
    }
    "parse escapes in a JsString" in {
      parse(""""\"\\/\b\f\n\r\t"""") === JsString("\"\\/\b\f\n\r\t")
      parse("\"L\\" + "u00e4nder\"") === JsString("Länder")
    }
    "properly parse a simple JsObject" in (
      parse(""" { "key" :42, "key2": "value" }""") ===
        JsObject("key" -> JsNumber(42), "key2" -> JsString("value"))
      )
    "properly parse a simple JsArray" in (
      parse("""[null, 1.23 ,{"key":true } ] """) ===
        JsArray(JsNull, JsNumber(1.23), JsObject("key" -> JsTrue))
      )
  }

  def parse(s: String): JsValue =
    JsonParser.Json.runWithContext(s, new JsonParser.Context) match {
      case Success(result)        => result
      case Failure(e: ParseError) => sys.error(e.format(s, new ErrorFormatter(showTraces = true)))
      case Failure(e)             => throw e
    }
}