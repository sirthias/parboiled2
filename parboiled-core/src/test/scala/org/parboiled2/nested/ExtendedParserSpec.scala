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

package org.parboiled2.nested

import scala.util.Success
import org.specs2.mutable.Specification

/**
 * Tests if a parser will compile when org.parboiled2._ is not explicitly imported
 */

class ExtendedParserSpec extends Specification {

  class FooParser(input: String) extends AbstractParser(input) {
    def Go = rule {
      foo ~ EOI
    }
  }

  "Parsers in files that dont explicitly import org.parboiled2._" should {
    "compile" in {
      new FooParser("foo123").Go.run() should_== Success("foo123")
    }
  }

}