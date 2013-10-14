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

package org.parboiled2

object ErrorUtils {
  import Parser.{ Error, Position, RuleStack }

  def formatError(input: ParserInput, error: Error): String = {
    val Error(Position(index, line, col), ruleStacks) = error
    val problem =
      if (index < input.length) s"Invalid input '${input charAt index}'"
      else "Unexpected end of input"

    problem + ", "
    s"expected ${ruleStacks.map(x ⇒ RuleStack(x.frames.reverse)) mkString ("\n", "\n\n", "\n")} " +
      s"(line $line, column $col): \n" +
      s"${input.getLine(line)}\n" + (" " * (col - 1)) + '^'
  }
}