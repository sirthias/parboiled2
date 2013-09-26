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

package org.parboiled2.examples

import org.parboiled2._
import scala.annotation.tailrec
import shapeless._

class SimpleCalculator1(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      (("+" ~ Term ~> ((_: Int) + _))
        | "-" ~ Term ~> ((_: Int) - _)))
  }

  def Term: Rule1[Int] = rule {
    Factor ~ zeroOrMore(
      "*" ~ Factor ~> ((_: Int) * _)
        | "/" ~ Factor ~> ((_: Int) / _))
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { "(" ~ Expression ~ ")" }

  def Number = rule { capture(Digits) ~> ((_: String).toInt) }

  def Digits = rule { oneOrMore(Digit) }

  def Digit = rule { "0" - "9" }
}

object SimpleCalculator1 {
  @tailrec
  def repl(): Unit = {
    val inputLine = readLine("--------------------------------------\nEnter expression for calculator (v2) > ")
    if (inputLine != "") {
      val simpleCalc = new SimpleCalculator1(inputLine)
      simpleCalc.run(_.InputLine) match {
        case Right(x)  ⇒ println(s"Expression is valid. Result: ${x}")
        case Left(err) ⇒ println(s"Expression is not valid. Error: ${ErrorUtils.formatError(inputLine, err)}")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}