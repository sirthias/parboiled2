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

import scala.annotation.switch
import spray.json.{ParserInput => _, _}
import org.parboiled2._

/**
 * This is a feature-complete JSON parser implementation that almost directly
 * models the JSON grammar presented at http://www.json.org as a parboiled2 PEG parser.
 */
object JsonParser extends Parser {
  import CharPredicate.{Digit, Digit19, HexDigit}
  import StringBuilding._

  class Context extends StringBuilding.Context

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"

  // the root rule
  val Json = rule { WhiteSpace ~ Value ~ EOI }

  val JsonObject: Rule1[JsObject] = rule {
    ws('{') ~ zeroOrMore(Pair).separatedBy(ws(',')) ~ ws('}') ~> ((fields: Seq[JsField]) => JsObject(fields :_*))
  }

  val Pair = rule { JsonStringUnwrapped ~ ws(':') ~ Value ~> ((_, _)) }

  val Value: Rule1[JsValue] = rule {
    // as an optimization of the equivalent rule:
    //   JsonString | JsonNumber | JsonObject | JsonArray | JsonTrue | JsonFalse | JsonNull
    // we make use of the fact that one-char look-ahead is enough to discriminate the cases
    run {
      state.cursorChar match {
        case '"' => JsonString
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => JsonNumber
        case '{' => JsonObject
        case '[' => JsonArray
        case 't' => JsonTrue
        case 'f' => JsonFalse
        case 'n' => JsonNull
        case _ => MISMATCH
      }
    }
  }

  val JsonString = rule { JsonStringUnwrapped ~> (JsString(_)) }

  val JsonStringUnwrapped = rule { '"' ~ clearSB ~ Characters ~ ws('"') ~ push(ctx.sb.toString) }

  val JsonNumber = rule { capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> (JsNumber(_)) ~ WhiteSpace }

  val JsonArray = rule { ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> (JsArray(_ :_*)) }

  val Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }

  val NormalChar = rule { !QuoteBackslash ~ ANY ~ appendLastChar }

  val EscapedChar = rule (
    QuoteSlashBackSlash ~ appendLastChar
      | 'b' ~ appendChar('\b')
      | 'f' ~ appendChar('\f')
      | 'n' ~ appendChar('\n')
      | 'r' ~ appendChar('\r')
      | 't' ~ appendChar('\t')
      | Unicode ~> { code => ctx.sb.append(code.asInstanceOf[Char]); () }
  )

  val Unicode = rule { 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }

  val Integer = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }

  val Digits = rule { oneOrMore(Digit) }

  val Frac = rule { "." ~ Digits }

  val Exp = rule { ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits }

  val JsonTrue = rule { "true" ~ WhiteSpace ~ push(JsTrue) }

  val JsonFalse = rule { "false" ~ WhiteSpace ~ push(JsFalse) }

  val JsonNull = rule { "null" ~ WhiteSpace ~ push(JsNull) }

  val WhiteSpace = rule { zeroOrMore(WhiteSpaceChar) }

  val ws = rule[Char]() { _ ~ WhiteSpace }
}