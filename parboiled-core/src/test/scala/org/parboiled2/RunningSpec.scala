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

import scala.util.{Success, Try}
import utest.*

object RunningSpec extends TestSuite {

  class TestParser(val input: ParserInput) extends Parser {
    def A                 = rule('a' ~ B ~ EOI)
    def B                 = rule(oneOrMore('b'))
    def C(n: Int)(m: Int) = rule((n - m).times('c'))
    def D[S <: String]    = rule('d')
    def go(): Try[Unit]   = null
  }

  val tests = Tests {

    "Running a rule should support several notations" - {

      "parser.rule.run()" - {
        val p = new TestParser("abb")
        p.A.run() ==> Success(())
      }

      "new Parser(...).rule.run()" - {
        new TestParser("abb").A.run() ==> Success(())
      }

      "parser.rule(args).run()" - {
        val p = new TestParser("ccc")
        p.C(4)(1).run() ==> Success(())
      }

      "parser.rule[targs].run()" - {
        val p = new TestParser("d")
        p.D[String].run() ==> Success(())
      }

      "this.rule.run()" - {
        val p = new TestParser("b") {
          override def go() = B.run()
        }
        p.go() ==> Success(())
      }

      "rule(B ~ EOI).run()" - {
        val p = new TestParser("bb") {
          override def go() = rule(B ~ EOI).run()
        }
        p.go() ==> Success(())
      }

      "namedRule(B ~ EOI).run()" - {
        val p = new TestParser("bb") {
          override def go() = namedRule("règle")(B ~ EOI).run()
        }
        p.go() ==> Success(())
      }
    }

  }
}
