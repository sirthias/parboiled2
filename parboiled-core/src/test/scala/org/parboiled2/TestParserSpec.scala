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

import org.specs2.matcher.MatchResult
import org.specs2.specification.{ NoToHtmlLinkFragments, Scope }
import org.specs2.mutable.Specification
import org.specs2.control.NoNumberOfTimes
import org.parboiled2.support.Unpack
import shapeless._

abstract class TestParserSpec extends Specification with NoToHtmlLinkFragments with NoNumberOfTimes {
  type TestParser0 = TestParser[HNil, Unit]
  type TestParser1[T] = TestParser[T :: HNil, T]
  type TestParserN[L <: HList] = TestParser[L, L]

  abstract class TestParser[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) extends Parser with Scope {
    type Context = Any

    def errorFormatter: ErrorFormatter = new ErrorFormatter(showTraces = true)
    def errorTraceCollectionLimit = 24

    def targetRule: RuleN[L]

    def beMatched = beTrue ^^ (parse(_: String).isRight)
    def beMatchedWith(r: Out) = parse(_: String) === Right(r)
    def beMismatched = beTrue ^^ (parse(_: String).isLeft)
    def beMismatchedWithError(pe: ParseError) = parse(_: String).left.toOption.get === pe
    def beMismatchedWithErrorMsg(msg: String): String ⇒ MatchResult[String] =
      input ⇒ parse(input).left.toOption.map(_.format(input, errorFormatter)).get === msg.stripMargin

    def parse(input: String): Either[ParseError, Out] = {
      import DeliveryScheme.Either
      targetRule.run(input, errorTraceCollectionLimit = errorTraceCollectionLimit)
    }
  }
}