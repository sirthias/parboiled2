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

import scala.util.Success
import org.specs2.mutable.Specification

class StringBuildingSpec extends Specification {

  object TestParser extends Parser {
    class Context extends StringBuilding.Context {
      var count = 0 // some custom member
    }

    val foo = rule {
      "foo" ~ run(ctx.count += 1) ~ 'X' ~ StringBuilding.appendLastChar ~ bar ~ EOI ~ push(ctx.sb.toString)
    }
    val bar = rule { "bar" ~ StringBuilding.appendString("BAR") }
  }

  "StringBuilding" should {

    "work as expected " in {
      TestParser.foo.runWithContext("fooXbar", new TestParser.Context) === Success("XBAR")
    }
  }
}
