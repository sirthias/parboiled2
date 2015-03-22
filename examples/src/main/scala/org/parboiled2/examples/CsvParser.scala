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

import scala.collection.immutable
import org.parboiled2._

object CsvParser extends {

  case class CsvFile(header: Option[Record], records: immutable.Seq[Record])
  case class Record(fields: immutable.Seq[String])
  case class Error(msg: String)

  /**
   * Simple, fast CSV parser.
   *
   * See http://tools.ietf.org/html/rfc4180#section-2
   */
  object Parser extends Parser {
    import StringBuilding._

    class Context(val headerPresent: Boolean, val fieldDelimiter: Char) extends StringBuilding.Context {
      val TEXTDATA = `TEXTDATA-BASE` -- fieldDelimiter
    }

    val `TEXTDATA-BASE` = CharPredicate.Printable -- '"'
    val QTEXTDATA = `TEXTDATA-BASE` ++ "\r\n"

    val file = rule {
      OWS ~ optional(test(ctx.headerPresent) ~ header ~ NL) ~
        oneOrMore(record).separatedBy(NL) ~ optional(NL) ~ EOI ~> CsvFile
    }

    val header = rule { record }

    val record = rule { oneOrMore(field).separatedBy(ctx.fieldDelimiter) ~> Record }

    val field = rule { `quoted-field` | `unquoted-field` }

    val `quoted-field` = rule {
      OWS ~ '"' ~ clearSB ~ zeroOrMore((QTEXTDATA | "\"\"") ~ appendLastChar) ~ '"' ~ OWS ~ push(ctx.sb.toString)
    }

    val `unquoted-field` = rule { capture(zeroOrMore(ctx.TEXTDATA)) }

    val NL = rule { optional('\r') ~ '\n' }

    val OWS = rule { zeroOrMore(' ') }
  }

  /**
   * Parses the given input into a [[CsvFile]] or an [[Error]] instance.
   */
  def apply(input: ParserInput, headerPresent: Boolean = true, fieldDelimiter: Char = ','): Either[Error, CsvFile] = {
    import DeliveryScheme.Either
    val context = new Parser.Context(headerPresent, fieldDelimiter)
    Parser.file.runWithContext(input, context).left.map(e => Error(e format input))
  }
}