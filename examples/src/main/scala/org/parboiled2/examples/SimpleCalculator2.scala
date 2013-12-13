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

abstract class Expr

case class Num(value: Int) extends Expr

case class BinOp(operator: Char, lhs: Expr, rhs: Expr) extends Expr

class SimpleCalculator2(val input: ParserInput) extends Parser {
  def InputLine: Rule1[Expr] = rule { Expression ~ EOI }

  def Expression: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
      "+" ~ Term ~> (BinOp('+', (_: Expr), _))
        | "-" ~ Term ~> (BinOp('-', (_: Expr), _)))
  }

  def Term: Rule1[Expr] = rule {
    Factor ~ zeroOrMore(
      "*" ~ Factor ~> (BinOp('*', (_: Expr), _))
        | "/" ~ Factor ~> (BinOp('/', (_: Expr), _)))
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { "(" ~ Expression ~ ")" }

  def Number = rule { capture(Digits) ~> ((x: String) ⇒ Num(x.toInt)) }

  def Digits = rule { oneOrMore(Digit) }

  def Digit = rule { "0" - "9" }
}

object SimpleCalculator2 {
  def interp(expr: Expr): Int = expr match {
    case Num(v)             ⇒ v
    case BinOp('+', e1, e2) ⇒ interp(e1) + interp(e2)
    case BinOp('-', e1, e2) ⇒ interp(e1) - interp(e2)
    case BinOp('*', e1, e2) ⇒ interp(e1) * interp(e2)
    case BinOp('/', e1, e2) ⇒ interp(e1) / interp(e2)
  }

  @tailrec
  def repl(): Unit = {
    val inputLine = readLine("--------------------------------------\nEnter expression for calculator (v2) > ")
    if (inputLine != "") {
      val simpleCalc = new SimpleCalculator2(inputLine)
      simpleCalc.InputLine() match {
        case Right(x)  ⇒ println(s"Expression is valid. Result: $x. Value: ${interp(x.head)}")
        case Left(err) ⇒ println(s"Expression is not valid. Error: ${simpleCalc.formatError(err)}")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}