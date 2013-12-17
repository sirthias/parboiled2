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
import scala.util.{ Success, Failure }
import org.parboiled2._

// `The classic non-context-free language <http://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples>`_
// .. math:: \{ a^n b^n c^n : n \ge 1 \}

class ABCParser(val input: ParserInput) extends Parser {

  def InputLine = rule {
    &(A ~ 'c') ~ oneOrMore('a') ~ B ~ !(ch('a') | 'b' | 'c') ~ EOI
  }

  def A: Rule0 = rule {
    'a' ~ optional(A) ~ 'b'
  }

  def B: Rule0 = rule {
    'b' ~ optional(B) ~ 'c'
  }
}

object ABCParser extends App {

  repl()

  @tailrec
  def repl(): Unit =
    readLine("---\nEnter expression for abc-parser > ") match {
      case "" ⇒
      case line ⇒
        val parser = new ABCParser(line)
        parser.InputLine.run() match {
          case Success(_)             ⇒ println("Expression is valid")
          case Failure(e: ParseError) ⇒ println("Expression is not valid: " + parser.formatError(e))
          case Failure(e)             ⇒ println(s"Unexpected error during parsing run: " + e)
        }
        repl()
    }
}