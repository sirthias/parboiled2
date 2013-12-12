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

class ErrorReportingSpec extends TestParserSpec {

  "The Parser should properly report errors" >> {

    "example 1" in new TestParser0 {
      import CharPredicate.UpperAlpha
      val hex = CharPredicate.UpperHexLetter

      def targetRule = rule {
        'a' ~ oneOrMore('b') ~ anyOf("cde") ~ ("fgh" | CharPredicate.Digit | hex | UpperAlpha) ~ EOI
      }

      "" must beMismatchedWithErrorMsg(
        """Unexpected end of input, expected 'a' (line 1, column 1):
          |
          |^
          |
          |1 rule mismatched at error location:
          |  targetRule / 'a'
          |""")

      "ax" must beMismatchedWithErrorMsg(
        """Invalid input 'x', expected 'b' (line 1, column 2):
          |ax
          | ^
          |
          |1 rule mismatched at error location:
          |  targetRule / oneOrMore / 'b'
          |""")

      "abx" must beMismatchedWithErrorMsg(
        """Invalid input 'x', expected 'b' or [cde] (line 1, column 3):
          |abx
          |  ^
          |
          |2 rules mismatched at error location:
          |  targetRule / oneOrMore / 'b'
          |  targetRule / [cde]
          |""")

      "abcx" must beMismatchedWithErrorMsg(
        """Invalid input 'x', expected 'f', Digit, hex or UpperAlpha (line 1, column 4):
          |abcx
          |   ^
          |
          |4 rules mismatched at error location:
          |  targetRule / | / | / | / "fgh" / 'f'
          |  targetRule / | / | / | / Digit
          |  targetRule / | / | / hex
          |  targetRule / | / UpperAlpha
          |""")
    }

  }
}