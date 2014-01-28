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
          |  targetRule / | / "fgh" / 'f'
          |  targetRule / | / Digit
          |  targetRule / | / hex
          |  targetRule / | / UpperAlpha
          |""")
    }

    "for rules with negative syntactic predicates" in new TestParser0 {
      def targetRule = rule { !"a" ~ ANY ~ !foo ~ EOI }
      def foo = rule { "bcd" }

      "a" must beMismatchedWithErrorMsg(
        """Invalid input 'a', expected !"a" (line 1, column 1):
          |a
          |^
          |
          |1 rule mismatched at error location:
          |  targetRule / ! / "a"
          |""")

      "xbcd" must beMismatchedWithErrorMsg(
        """Invalid input 'b', expected !(foo) (line 1, column 2):
          |xbcd
          | ^
          |
          |1 rule mismatched at error location:
          |  targetRule / ! / (foo)
          |""")
    }

    "for rules with backtick identifiers" in new TestParser0 {
      val `this:that` = CharPredicate.Alpha
      def targetRule = rule { `foo-bar` ~ `this:that` ~ EOI }
      def `foo-bar` = rule { 'x' }

      "a" must beMismatchedWithErrorMsg(
        """Invalid input 'a', expected foo-bar (line 1, column 1):
          |a
          |^
          |
          |1 rule mismatched at error location:
          |  targetRule / foo-bar
          |""")

      "x" must beMismatchedWithErrorMsg(
        """Unexpected end of input, expected this:that (line 1, column 2):
          |x
          | ^
          |
          |1 rule mismatched at error location:
          |  targetRule / this:that
          |""")
    }

    "if the error location is the newline at line-end" in new TestParser0 {
      def targetRule = rule { "abc" ~ EOI }

      "ab\nc" must beMismatchedWithErrorMsg(
        """Invalid input '\n', expected 'c' (line 1, column 3):
          |ab
          |  ^
          |
          |1 rule mismatched at error location:
          |  targetRule / "abc" / 'c'
          |""")
    }
  }
}