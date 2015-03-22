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

package org.parboiled2.nestedpackage

import scala.util.Success
import org.specs2.mutable.Specification
import org.parboiled2.CharPredicate

class AlienPackageParserSpec extends Specification {

  object AbstractParser extends org.parboiled2.Parser {
    type Context = Unit // just for fun
    def foo = rule { capture("foo" ~ zeroOrMore(CharPredicate.Digit)) }
  }

  object FooParser extends org.parboiled2.SimpleParser {
    import AbstractParser.foo

    def Go = rule { foo ~ EOI }
  }

  "Parsers in files that dont explicitly import org.parboiled2._" should {
    "compile" in {
      FooParser.Go.runWithContext("foo123", ()) === Success("foo123")
    }
  }
}
