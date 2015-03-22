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

import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scala.io.StdIn
import org.parboiled2._

object Calculator1 extends App {

  /**
   * This parser reads simple calculator expressions and evaluates them right during
   * the parsing run with the help of the value stack.
   */
  object Parser extends SimpleParser {
    val InputLine = rule { Expression ~ EOI }

    val Expression: Rule1[Int] = rule {
      Term ~ zeroOrMore(
        '+' ~ Term ~> ((_: Int) + _)
          | '-' ~ Term ~> ((_: Int) - _))
    }

    val Term = rule {
      Factor ~ zeroOrMore(
        '*' ~ Factor ~> ((_: Int) * _)
          | '/' ~ Factor ~> ((_: Int) / _))
    }

    val Factor = rule { Number | Parens }

    val Parens = rule { '(' ~ Expression ~ ')' }

    val Number = rule { capture(Digits) ~> (_.toInt) }

    val Digits = rule { oneOrMore(CharPredicate.Digit) }
  }

  repl()

  @tailrec
  def repl(): Unit =
    StdIn.readLine("---\nEnter calculator expression > ") match {
      case "" =>
      case line =>
        Parser.InputLine.run(line) match {
          case Success(result)        => println("Result: " + result)
          case Failure(e: ParseError) => println("Expression is not valid: " + e.format(line))
          case Failure(e)             => println("Unexpected error during parsing run: " + e)
        }
        repl()
    }
}