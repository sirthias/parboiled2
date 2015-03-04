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

import org.parboiled2.Parser
import org.specs2.mutable.Specification


class SimpleSqlParserSpec extends Specification {
  import Parser.DeliveryScheme.Throw

  val sample =
    """
      |--comment comment
      |
      |create table tables (
      |  id int identity not null,
      |  label varchar(15) not null,
      |  location int not null
      |)
      |
      |create table locations(
      |  id int identity not null,
      |  name varchar(15) not null,
      |  owner varchar(50) not null
      |)
      |
      |-- more comments
    """.stripMargin


  "The SimpleSqlParser" should {
    "correctly parse simple create table Sql script" in {

      val delimeter = "  "

      SimpleSqlParser(sample, delimeter).DDL.run() === Vector(
        DbTable("tables", Vector("id", "label", "location")),
        DbTable("locations", Vector("id", "name", "owner")))

    }

  }


}
