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

package org.parboiled2

import utest._

object CustomCharAndStringExpansionSpec extends TestParserSpec {

  val tests = Tests {

    "The parser" - {

      "allow for custom char expansion" - new TestParser0 {
        implicit def chWithX(c: Char): Rule0 =
          if (c == EOI) rule(ch(EOI)) else rule(ch(c) ~ ch('x'))

        def targetRule = rule('a' ~ 'b' ~ EOI)

        "axbx" must beMatched
        "ab" must beMismatched
      }

      "allow for custom string expansion" - new TestParser0 {
        implicit def wspStr(s: String): Rule0 =
          rule {
            str(s) ~ zeroOrMore(' ')
          }

        def targetRule = rule("foo" ~ "bar" ~ EOI)

        "foobar" must beMatched
        "foo   bar" must beMatched
        "foo" must beMismatched
      }
    }
  }
}
