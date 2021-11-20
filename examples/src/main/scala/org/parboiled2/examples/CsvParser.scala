/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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

object CsvParser {

  case class CsvFile(header: Option[Record], records: immutable.Seq[Record])
  case class Record(fields: immutable.Seq[String])
  case class Error(msg: String)

  /** Parses the given input into a [[CsvFile]] or an [[Error]] instance.
    */
  def apply(input: ParserInput, headerPresent: Boolean = true, fieldDelimiter: Char = ','): Either[Error, CsvFile] = {
    import Parser.DeliveryScheme.Either
    val parser = new CsvParser(input, headerPresent, fieldDelimiter)
    parser.file.run().left.map(error => Error(parser.formatError(error)))
  }

  private val `TEXTDATA-BASE` = CharPredicate.Printable -- '"'
  private val QTEXTDATA       = `TEXTDATA-BASE` ++ "\r\n"
}

/** Simple, fast CSV parser.
  *
  * See http://tools.ietf.org/html/rfc4180#section-2
  */
class CsvParser(val input: ParserInput, headerPresent: Boolean, fieldDelimiter: Char)
    extends Parser with StringBuilding {
  import CsvParser._

  val TEXTDATA = `TEXTDATA-BASE` -- fieldDelimiter

  def file =
    rule {
      OWS ~ optional(test(headerPresent) ~ header ~ NL) ~ oneOrMore(record)
        .separatedBy(NL) ~ optional(NL) ~ EOI ~> CsvFile.apply _
    }

  def header = rule(record)

  def record = rule(oneOrMore(field).separatedBy(fieldDelimiter) ~> Record.apply _)

  def field = rule(`quoted-field` | `unquoted-field`)

  def `quoted-field` =
    rule {
      OWS ~ '"' ~ clearSB() ~ zeroOrMore((QTEXTDATA | "\"\"") ~ appendSB()) ~ '"' ~ OWS ~ push(sb.toString)
    }

  def `unquoted-field` = rule(capture(zeroOrMore(TEXTDATA)))

  def NL = rule(optional('\r') ~ '\n')

  def OWS = rule(zeroOrMore(' '))
}
