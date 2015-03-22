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

import java.lang.{ StringBuilder ⇒ JStringBuilder }

/**
 * For certain high-performance use-cases it is better to construct Strings
 * that the parser is to produce/extract from the input in a char-by-char fashion.
 *
 * The object defines helper rules for this.
 * Just `import StringBuilding._` and make sure that your parser context mixes in the
 * `StringBuilding.Context` trait.
 */
object StringBuilding extends Parser {

  trait Context {
    val sb = new JStringBuilder
  }

  val clearSB: Rule0 = rule { run(ctx.sb.setLength(0)) }

  val appendLastChar: Rule0 = rule { run(ctx.sb.append(state.lastChar)) }

  val appendChar: Rule10[Char] = rule[Char]() { c ⇒ run(ctx.sb.append(c)) }

  val appendString: Rule10[String] = rule[String]() { s ⇒ run(ctx.sb.append(s)) }

  val prependLastChar: Rule0 = rule { run(doPrepend(ctx.sb, state.lastChar)) }

  val prependChar: Rule10[Char] = rule[Char]() { c ⇒ run(doPrepend(ctx.sb, c)) }

  val prependString: Rule10[String] = rule[String]() { s ⇒ run(doPrepend(ctx.sb, s)) }

  val setSB: Rule10[String] = rule[String]() { s ⇒ run(doSet(ctx.sb, s)) }

  private def doPrepend(sb: JStringBuilder, c: Char): Unit = {
    val saved = sb.toString
    sb.setLength(0)
    sb.append(c)
    sb.append(saved)
  }

  private def doPrepend(sb: JStringBuilder, s: String): Unit = {
    val saved = sb.toString
    sb.setLength(0)
    sb.append(s)
    sb.append(saved)
  }

  private def doSet(sb: JStringBuilder, s: String): Unit = {
    sb.setLength(0)
    sb.append(s)
  }
}
