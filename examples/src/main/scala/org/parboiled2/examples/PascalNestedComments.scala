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

class PascalNestedComments(val input: ParserInput) extends Parser {
  def InputLine: Rule0 = rule { C ~ EOI }

  def BeginComment = rule { "(*" }

  def EndComment = rule { "*)" }

  def C: Rule0 = rule { BeginComment ~ zeroOrMore(N) ~ EndComment }

  def N = rule { C | (!BeginComment ~ !EndComment ~ Z) }

  def Z = rule { "a" - "z" | "A" - "Z" | "0" - "9" }
}

object PascalNestedComments {
  @tailrec
  def repl(): Unit = {
    val inputLine = readLine("--------------------------------------\nEnter expression for Pascal-nested-comments parser (v2) > ")
    if (inputLine != "") {
      val pascalNestedComments = new PascalNestedComments(inputLine)
      pascalNestedComments.InputLine() match {
        case Right(_)  ⇒ println("Expression is valid")
        case Left(err) ⇒ println(s"Expression is not valid. Error: ${pascalNestedComments.formatError(err)}")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}