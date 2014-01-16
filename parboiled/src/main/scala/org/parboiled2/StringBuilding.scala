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

/**
 * For certain high-performance use-cases it is better to construct Strings
 * that the parser is to produce/extract from the input in a char-by-char fashion.
 *
 * Mixing this trait into your parser gives you a simple facility to support this.
 */
trait StringBuilding { this: Parser ⇒
  protected val sb = new java.lang.StringBuilder

  def clearSB(): Rule0 = rule { run(sb.setLength(0)) }

  def append(): Rule0 = rule { run(sb.append(lastChar)) }

  def append(offset: Int): Rule0 = rule { run(sb.append(charAt(offset))) }

  def append(c: Char): Rule0 = rule { run(sb.append(c)) }

  def append(s: String): Rule0 = rule { run(sb.append(s)) }
}
