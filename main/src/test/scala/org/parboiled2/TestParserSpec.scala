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
import shapeless._

abstract class TestParserSpec extends Specification {
  import Parser.{ Result, Value, Error }

  type TestParser0 = TestParser[HNil]

  abstract class TestParser[L <: HList] extends Parser with Scope {
    def beMatched = beTrue ^^ (parse(_: String).isRight)
    def beMatchedWith(r: L) = parse(_: String) === Right(r)
    def beValueOf(r: L) = (_: Result[L]) === Value(r)
    def beMatchedBy[T](value: T)(implicit ev: (T :: HNil) <:< L) = beMatchedWith(value :: HNil)
    def beMismatched = beTrue ^^ (parse(_: String).isLeft)
    def beMismatchedWithError(pe: Error) = parse(_: String) === Left(pe)

    var input: ParserInput = _
    def targetRule: RuleN[L]

    def parse(input: String): Either[Error, L] = {
      this.input = input
      run(_.targetRule)
    }

    def startParsing(input: String, lastChunk: Boolean): Result[L] = {
      this.input = input
      startParsing(_.targetRule, lastChunk)
    }
  }
}
