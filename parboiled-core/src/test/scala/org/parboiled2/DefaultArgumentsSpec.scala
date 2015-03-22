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

import org.specs2.mutable._
import scala.util.Success

class DefaultArgumentsSpec extends Specification {

  case class A(a: Int = 0, b: Int = 1)

  object Foo extends SimpleParser {
    def Foo: Rule1[A] = rule { "foo" ~ push(A(b = 2)) }
  }

  "The `push` action" should {
    "properly handle default arguments" in {
      Foo.Foo.run("foo") === Success(A(0, 2))
    }
  }
}