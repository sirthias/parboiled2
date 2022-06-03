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

import org.parboiled2.support.Unpack
import org.parboiled2.support.hlist._
import utest._

abstract class TestParserSpec extends TestSuite {
  type TestParser0             = TestParser[HNil, Unit]
  type TestParser1[T]          = TestParser[T :: HNil, T]
  type TestParserN[L <: HList] = TestParser[L, L]

  abstract class TestParser[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) extends Parser {
    var input: ParserInput             = _
    def errorFormatter: ErrorFormatter = new ErrorFormatter(showTraces = true)

    def targetRule: RuleN[L]

    // shadow utests implicit extension on Strings which collides with our `str2CharRangeSupport`
    def TestableString: Any = null

    sealed trait MustAssert {
      def assert(str: String): Unit
    }

    private case class BeMatchedWith(underlying: String => Unit) extends MustAssert {
      override def assert(str: String): Unit = underlying(str)
    }
    def beMatched: MustAssert                             = BeMatchedWith(assertMatched)
    def beMatchedWith(r: Out): MustAssert                 = BeMatchedWith(assertMatchedWith(r))
    def beMismatched: MustAssert                          = BeMatchedWith(assertMismatched)
    def beMismatchedWithErrorMsg(msg: String): MustAssert = BeMatchedWith(assertMismatchedWithErrorMsg(msg))

    implicit class StringExt(str: String) {
      def must(mustAssert: MustAssert): Unit = mustAssert.assert(str)
    }

    def assertMatched(str: String): Unit             = assert(parse(str).isRight)
    def assertMatchedWith(r: Out)(str: String): Unit = assert(parse(str) == Right(r))
    def assertMismatched(str: String): Unit          = assert(parse(str).isLeft)

    // def beMismatchedWithError(pe: ParseError) = parse(_: String).left.toOption.get === pe
    def assertMismatchedWithErrorMsg(expected: String)(str: String): Unit = {
      val actual = parse(str).left.toOption.map(formatError(_, errorFormatter)).get
      val expct  = expected.stripMargin
      assert(actual == expct)
    }

    def parse(input: String): Either[ParseError, Out] = {
      this.input = input
      import Parser.DeliveryScheme.Either
      targetRule.run()
    }
  }
}
