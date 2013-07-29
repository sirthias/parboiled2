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

package org.parboiled2.calculators.v2

import org.parboiled2._
import scala.annotation.tailrec
import org.parboiled.scala.parserunners.ReportingParseRunner

class SimpleCalculator(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule = rule { Term ~ zeroOrMore((ch('+') | '-') ~ Term) }

  def Term = rule { Factor ~ zeroOrMore((ch('*') | '/') ~ Factor) }

  def Factor = rule { Digits | Parens }

  def Parens = rule { "(" ~ Expression ~ ")" }

  def Digits = rule { oneOrMore(Digit) }

  def Digit = rule { (ch('0') | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') }
}

object CalculatorExpressionVerifier {
  @tailrec
  def repl(): Unit = {
    print("--------------------------------------\n")
    print("Enter expression for calculator (v2) > ")
    val inputLine = readLine()
    if (inputLine != "") {
      val simpleCalc = new SimpleCalculator(inputLine)
      simpleCalc.run(simpleCalc.InputLine) match {
        case Right(_) => println("Expression is valid")
        case Left(err) => println(s"Expression is not valid. Error: ${ErrorUtils.formatError(inputLine, err)}")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}
