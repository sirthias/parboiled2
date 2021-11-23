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

package org.parboiled2.nestedpackage

import scala.util.Success
import utest._

object AlienPackageParserSpec extends TestSuite {

  abstract class AbstractParser(val input: org.parboiled2.ParserInput) extends org.parboiled2.Parser {
    import org.parboiled2.{CharPredicate, Rule1}

    def foo: Rule1[String] = rule(capture("foo" ~ zeroOrMore(CharPredicate.Digit)))
  }

  class FooParser(input: String) extends AbstractParser(input) {
    def Go = rule(foo ~ EOI)
  }

  val tests = Tests {
    "Parsers in files that dont explicitly import org.parboiled2._" - {
      "compile" - {
        new FooParser("foo123").Go.run() ==> Success("foo123")
      }
    }
  }
}
