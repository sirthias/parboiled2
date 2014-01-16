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

object CharUtils {
  /**
   * Returns the int value of a given hex digit char.
   * Note: this implementation is very fast (since it's branchless) and therefore
   * does not perform ANY range checks!
   */
  def hexValue(c: Char): Int = (c & 0x1f) + ((c >> 6) * 0x19) - 0x10

  /**
   * Returns the lower-case hex digit corresponding to the last 4 bits of the given Long.
   * (fast branchless implementation)
   */
  def lowerHexDigit(long: Long): Char = lowerHexDigit_internal((long & 0x0FL).toInt)

  /**
   * Returns the lower-case hex digit corresponding to the last 4 bits of the given Int.
   * (fast branchless implementation)
   */
  def lowerHexDigit(int: Int): Char = lowerHexDigit_internal(int & 0x0F)

  private def lowerHexDigit_internal(i: Int) = (48 + i + (39 & ((9 - i) >> 31))).toChar

  /**
   * Returns the upper-case hex digit corresponding to the last 4 bits of the given Long.
   * (fast branchless implementation)
   */
  def upperHexDigit(long: Long): Char = upperHexDigit_internal((long & 0x0FL).toInt)

  /**
   * Returns the upper-case hex digit corresponding to the last 4 bits of the given Int.
   * (fast branchless implementation)
   */
  def upperHexDigit(int: Int): Char = upperHexDigit_internal(int & 0x0F)

  private def upperHexDigit_internal(i: Int) = (48 + i + (7 & ((9 - i) >> 31))).toChar

  /**
   * Efficiently lower-cases the given character.
   * Note: only works for 7-bit ASCII letters.
   */
  def toLowerCase(c: Char): Char = if (CharPredicate.UpperAlpha(c)) (c + 0x20).toChar else c

  /**
   * Efficiently upper-cases the given character.
   * Note: only works for 7-bit ASCII letters.
   */
  def toUpperCase(c: Char): Char = if (CharPredicate.LowerAlpha(c)) (c + 0x20).toChar else c

  def escape(c: Char): String = c match {
    case '\t'                           ⇒ "\\t"
    case '\r'                           ⇒ "\\r"
    case '\n'                           ⇒ "\\n"
    case EOI                            ⇒ "EOI"
    case x if Character.isISOControl(x) ⇒ "\\u%04x" format c.toInt
    case x                              ⇒ x.toString
  }

  val escapedChars = CharPredicate("\t\r\n", EOI, Character.isISOControl _)

  def escape(s: String): String =
    if (escapedChars.matchesAny(s)) s.flatMap(escape(_: Char)) else s
}
