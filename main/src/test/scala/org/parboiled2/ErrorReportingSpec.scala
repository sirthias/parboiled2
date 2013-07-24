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

class ErrorReportingSpec extends SimpleCalculatorSpec {
  "Error reporting" should {
    "compose error messages for simple expression" in new TestParser with ErrorUtils {
      def targetRule = rule { oneOrMore("x") ~ ch('a') ~ 'b' ~ 'e' }
      def parser = this

      "xxxace" must Mismatch
      error must beSome
      error map { err =>
        err.mark mustEqual Mark(0, 4, 4)
        err.expectedRules must haveTheSameElementsAs(List("b"))
      }
    }

    "compose error messages for simple calculator" in new SimpleCalculator with ErrorUtils {
      def targetRule = InputLine
      def parser = this

      "3+*5" must Mismatch
      error must beSome
      error map { err =>
        err.mark mustEqual Mark(0, 2, 2)
        err.expectedRules must haveTheSameElementsAs(List("SimpleCalculator.Term"))
      }
    }

    "track lines numbers" in {
      "zero line" in new TestParser with ErrorUtils {
        def targetRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }
        def parser = this

        "x\nb\nc" must Mismatch
        error must beSome
        error map { err =>
          err.mark mustEqual Mark(0, 0, 0)
        }
      }

      "first line" in new TestParser with ErrorUtils {
        def targetRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }
        def parser = this

        "a\nx\nc" must Mismatch
        error must beSome
        error map { err =>
          err.mark mustEqual Mark(1, 1, 2)
        }
      }

      "second line" in new TestParser with ErrorUtils {
        def targetRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }
        def parser = this

        "a\nb\nx" must Mismatch
        error must beSome
        error map { err =>
          err.mark mustEqual Mark(2, 1, 4)
        }
      }
    }

    "correctly process FirstOf" in {
      "producing no errors for first alternative" in new TestParser with ErrorUtils {
        def targetRule: Rule = rule { ch('a') | 'b' }
        def parser = this

        "aaa" must Match
        error must beNone
      }

      "producing no errors for second alternative" in new TestParser with ErrorUtils {
        def targetRule: Rule = rule { ch('a') | 'b' }
        def parser = this

        "b" must Match
        error must beNone
      }
    }
  }
}