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

import org.scalacheck.{Gen, Prop, Test}
import org.scalacheck.util.Pretty
import org.scalacheck.Prop.forAll
import utest._

object CharUtilsSpec extends TestSuite with UTestScalaCheck {

  val hexChars = for (i <- Gen.choose(0, 15)) yield i -> Integer.toHexString(i).charAt(0)

  val tests = Tests {

    "CharUtils" - {
      "hexValue" - forAll(hexChars) { case (i, c) =>
        CharUtils.hexValue(c) == i
      }.checkUTest()
      "numberOfHexDigits" - forAll { (l: Long) =>
        CharUtils.numberOfHexDigits(l) == java.lang.Long.toHexString(l).length
      }
        .checkUTest()
      "upperHexString" - forAll { (l: Long) =>
        CharUtils.upperHexString(l) == java.lang.Long.toHexString(l).toUpperCase
      }
        .checkUTest()
      "lowerHexString" - forAll((l: Long) => CharUtils.lowerHexString(l) == java.lang.Long.toHexString(l))
        .checkUTest()
      "numberOfDecimalDigits" - forAll { (l: Long) =>
        CharUtils.numberOfDecimalDigits(l) == java.lang.Long.toString(l).length
      }.checkUTest()
      "signedDecimalString" - forAll((l: Long) => CharUtils.signedDecimalString(l) == java.lang.Long.toString(l))
        .checkUTest()
    }
  }
}

// from https://github.com/lihaoyi/utest/issues/2#issuecomment-67300735
trait UTestScalaCheck {

  protected[this] object UTestReporter extends Test.TestCallback {
    private val prettyParams = Pretty.defaultParams

    override def onTestResult(name: String, res: org.scalacheck.Test.Result): Unit = {
      val scalaCheckResult = if (res.passed) "" else Pretty.pretty(res, prettyParams)
      assert(scalaCheckResult.isEmpty)
    }
  }

  implicit protected[this] class PropWrapper(prop: Prop) {

    def checkUTest(): Unit =
      prop.check(Test.Parameters.default.withTestCallback(UTestReporter))
  }

}
