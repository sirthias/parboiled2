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

object ABCParser extends App {
  repl()

  @tailrec
  def repl(): Unit = {
    // TODO: Replace next three lines with `scala.Predef.readLine(text: String, args: Any*)`
    // once BUG https://issues.scala-lang.org/browse/SI-8167 is fixed
    print("---\nEnter expression for abc-parser > ")
    Console.out.flush()
    readLine() match {
      case "" ⇒
      case line ⇒
        val parser = new ABCParser(line)
        parser.InputLine.run() match {
          case Success(_)             ⇒ println("Expression is valid")
          case Failure(e: ParseError) ⇒ println("Expression is not valid: " + parser.formatError(e))
          case Failure(e)             ⇒ println("Unexpected error during parsing run: " + e)
        }
        repl()
    }
  }
}

/**
 * This parser reads the classic non-context-free language:
 *
 *     a^n b^n c^n (for n > 1)
 *
 * See also: http://en.wikipedia.org/wiki/Parsing_expression_grammar#Examples
 */
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