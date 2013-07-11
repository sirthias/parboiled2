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

package org.parboiled

import org.specs2.specification.Scope
import org.specs2.mutable.Specification

trait TestParserComponent { _: Specification ⇒
  def Match = beTrue
  def Mismatch = beFalse
}

abstract class TestParser extends Parser with Scope {
  var input: ParserInput = _
  def testRule: Rule

  def parse(input: String) = {
    this.input = input
    val marker = mark
    val matched = testRule.matched
    reset(marker)
    matched
  }
}
