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

package org.parboiled2.examples.v1

import org.parboiled.scala._
import org.parboiled.scala.parserunners.ReportingParseRunner
import scala.annotation.tailrec
import org.parboiled.errors.ErrorUtils

// NOTE: https://github.com/sirthias/parboiled/blob/master/examples-scala/src/main/scala/org/parboiled/examples/calculators/SimpleCalculator0.scala
class SimpleCalculator extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule0 = rule { Term ~ zeroOrMore(anyOf("+-") ~ Term) }

  def Term = rule { Factor ~ zeroOrMore(anyOf("*/") ~ Factor) }

  def Factor = rule { Digits | Parens }

  def Parens = rule { "(" ~ Expression ~ ")" }

  def Digits = rule { oneOrMore(Digit) }

  def Digit = rule { "0" - "9" }
}

object CalculatorExpressionVerifier {
  val simpleCalc = new SimpleCalculator

  @tailrec
  def repl(): Unit = {
    print("Enter expression for calculator (v1) > ")
    val inputLine = readLine()
    if (inputLine != "") {
      val result = ReportingParseRunner(simpleCalc.InputLine).run(inputLine)
      if (result.matched) {
        println("Expression is valid")
      } else {
        println(s"Expression is not valid. Errors: ${ErrorUtils.printParseErrors(result)}")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}