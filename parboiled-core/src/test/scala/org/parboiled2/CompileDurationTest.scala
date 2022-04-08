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

import org.parboiled2.support.hlist.{::, HNil}

object CompileDurationTest extends TestParserSpec {

  val tests = Tests {

    "The Parser should compile" - {
      def combine6(a1: String, a2: String, a3: String, a4: String, a5: String, a6: String): String =
        a1 + a2 + a3 + a4 + a5 + a6

      "`~` combinator" - new TestParser1[String] {
        def basicRule: Rule[HNil, String :: HNil] = rule(capture("a"))
        def targetRule = rule(
          basicRule ~ basicRule ~ basicRule ~ basicRule ~ basicRule ~ basicRule ~> combine6 _
        )

        "" must beMismatched
        "aaaaaa" must beMatched
        "ac" must beMismatched
        "a" must beMismatched
        "b" must beMismatched
      }

    }
  }
}
