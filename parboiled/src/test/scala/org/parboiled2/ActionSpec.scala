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

import shapeless._

class ActionSpec extends TestParserSpec {

  "The Parser should correctly handle the" >> {

    "`capture`" in new TestParser1[String] {
      def targetRule = rule { 'a' ~ capture(zeroOrMore('b')) ~ EOI }
      "a" must beMatchedWith1("")
      "b" must beMismatched
      "ab" must beMatchedWith1("b")
      "abb" must beMatchedWith1("bb")
    }

    "`test`" in new TestParser0 {
      var flag = true
      def targetRule = rule { test(flag) }
      "x" must beMatched
      flag = false
      "x" must beMismatched
    }

    "`push` (example 1)" in new TestParser1[String] {
      def targetRule = rule { 'x' ~ push(()) ~ push(HNil) ~ 'y' ~ push("yeah") ~ EOI }
      "xy" must beMatchedWith1("yeah")
    }

    "`push` (example 2)" in new TestParser[Int :: Double :: Long :: String :: HNil] {
      def targetRule = rule { 'x' ~ push(42 :: 3.14 :: HNil) ~ push(0L :: "yeah" :: HNil) ~ EOI }
      "x" must beMatchedWith(42 :: 3.14 :: 0L :: "yeah" :: HNil)
      "y" must beMismatched
    }

  }
}