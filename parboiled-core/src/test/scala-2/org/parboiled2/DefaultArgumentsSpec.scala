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

import scala.util.Success

object DefaultArgumentsSpec extends TestSuite {

  case class A(a: Int = 0, b: Int = 1)

  class Foo(val input: ParserInput) extends Parser {
    def Foo: Rule1[A] = rule("foo" ~ push(A(b = 2)))
  }

  val tests = Tests {
    "The `push` action" - {
      "properly handle default arguments" - {
        new Foo("foo").Foo.run() ==> Success(A(0, 2))
      }
    }
  }
}
