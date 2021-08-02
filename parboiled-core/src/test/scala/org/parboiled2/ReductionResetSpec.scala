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
import org.parboiled2.support.hlist._

object ReductionResetSpec extends TestParserSpec {

  abstract class ReductionResetParser extends TestParser1[Int] {

    def Alternative: Rule1[Int] =
      rule {
        Digits ~ (String2Int ~ "." | String2Int)
      }

    def ZeroOrMore: Rule1[Int] =
      rule {
        Digits ~ zeroOrMore(String2Int ~ "." ~ Int2String) ~ String2Int
      }

    def OneOrMore: Rule1[Int] =
      rule {
        Digits ~ oneOrMore(String2Int ~ "." ~ Int2String) ~ String2Int
      }

    def String2Int: Rule[String :: HNil, Int :: HNil] =
      rule {
        run((_: String).toInt)
      }

    def Int2String: Rule[Int :: HNil, String :: HNil] =
      rule {
        run((_: Int).toString)
      }

    def Digits: Rule1[String] =
      rule {
        capture(oneOrMore(CharPredicate.Digit))
      }
  }

  val tests = Tests {

    "ReductionResetSpec" - {

      "reduction reset in `|`" - new ReductionResetParser {
        def targetRule = Alternative

        // re-enable after fixing
        //"123" must beMatchedWith(123)
      }

      "reduction reset in `zeroOrMore`" - new ReductionResetParser {
        def targetRule = ZeroOrMore

        //"123" must beMatchedWith(123)
      }

      "reduction reset in `oneOrMore`" - new ReductionResetParser {
        def targetRule = OneOrMore

        //"123." must beMatchedWith(123)
      }
    }
  }
}
