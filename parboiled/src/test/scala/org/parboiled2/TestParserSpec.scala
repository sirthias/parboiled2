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

import org.specs2.specification.{ NoToHtmlLinkFragments, Scope }
import org.specs2.mutable.Specification
import shapeless._
import org.specs2.control.NoNumberOfTimes

abstract class TestParserSpec extends Specification with NoToHtmlLinkFragments with NoNumberOfTimes {
  type TestParser0 = TestParser[HNil]
  type TestParser1[T] = TestParser[T :: HNil]

  abstract class TestParser[L <: HList] extends Parser with Scope {
    def beMatched = beTrue ^^ (parse(_: String).isRight)
    def beMatchedWith(r: L) = parse(_: String) === Right(r)
    def beMatchedWith1[T](value: T)(implicit ev: (T :: HNil) <:< L) = beMatchedWith(value :: HNil)
    def beMismatched = beTrue ^^ (parse(_: String).isLeft)
    def beMismatchedWithError(pe: ParseError) = parse(_: String).left.toOption.get === pe
    def beMismatchedWithErrorMsg(msg: String) =
      parse(_: String).left.toOption.map(formatError(_, showTraces = true)).get === msg.stripMargin

    var input: ParserInput = _
    def targetRule: RuleN[L]

    def parse(input: String): Parser.Result[L] = {
      this.input = input
      targetRule()
    }
  }
}