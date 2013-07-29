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

import org.specs2.specification.Scope
import org.specs2.mutable.Specification

abstract class TestParserSpec extends Specification {
  trait TestParser extends Parser with Scope {
    def Match = beTrue ^^ (parse(_))
    def Mismatch = beFalse ^^ (parse(_))

    var input: ParserInput = _
    def targetRule: Rule

    def parse(input: String) = {
      this.input = input
      val marker = mark
      val matched = targetRule.matched
      reset(marker)
      matched
    }

    lazy val run = super.run(targetRule)
  }
}
