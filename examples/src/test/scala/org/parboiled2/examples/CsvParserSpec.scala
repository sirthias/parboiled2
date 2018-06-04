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

import org.parboiled2.examples.CsvParser.{ Record, CsvFile }
import org.specs2.mutable.Specification

class CsvParserSpec extends Specification {

  "The CsvParser" should {
    "correctly parse simple CSV test input" in {
      CsvParser {
        """"first_name","last_name","company_name","address","city","county","state","zip","phone1","phone2","email","web"
          |"James","Butt", "Benton, John B Jr","6649 N Blue Gum St","New Orleans","Orleans","LA",70116,"504-621-8927","504-845-1427","jbutt@gmail.com","http://www.bentonjohnbjr.com"
          |"Josephine","Darakjy","Chanay, Jeffrey A Esq","4 B Blue Ridge Blvd","Brighton","Livingston","MI",48116,"810-292-9388","810-374-9840","josephine_darakjy@darakjy.org","http://www.chanayjeffreyaesq.com"
          |Art,"Venere","Chemel, James L Cpa","8 W Cerritos Ave #54","Bridgeport","Gloucester","NJ",08014 ,"856-636-8749","856-264-4130","art@venere.org","http://www.chemeljameslcpa.com"
          |"Lenna","Paprocki","Feltz ""Printing"" Service", 639 Main St,"Anchorage","Anchorage","AK",99501,"907-385-4412","907-921-2010","lpaprocki@hotmail.com","http://www.feltzprintingservice.com"
          |""".stripMargin
      } === file(
        record("first_name", "last_name", "company_name", "address", "city", "county", "state", "zip", "phone1", "phone2", "email", "web"),
        record("James", "Butt", "Benton, John B Jr", "6649 N Blue Gum St", "New Orleans", "Orleans", "LA", "70116", "504-621-8927", "504-845-1427", "jbutt@gmail.com", "http://www.bentonjohnbjr.com"),
        record("Josephine", "Darakjy", "Chanay, Jeffrey A Esq", "4 B Blue Ridge Blvd", "Brighton", "Livingston", "MI", "48116", "810-292-9388", "810-374-9840", "josephine_darakjy@darakjy.org", "http://www.chanayjeffreyaesq.com"),
        record("Art", "Venere", "Chemel, James L Cpa", "8 W Cerritos Ave #54", "Bridgeport", "Gloucester", "NJ", "08014 ", "856-636-8749", "856-264-4130", "art@venere.org", "http://www.chemeljameslcpa.com"),
        record("Lenna", "Paprocki", "Feltz \"Printing\" Service", " 639 Main St", "Anchorage", "Anchorage", "AK", "99501", "907-385-4412", "907-921-2010", "lpaprocki@hotmail.com", "http://www.feltzprintingservice.com"))
    }
  }

  def file(header: Record, records: Record*) = Right(CsvFile(Some(header), records.toVector))
  def record(fields: String*) = Record(fields.toVector)
}