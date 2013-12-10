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

import java.nio.charset.Charset

sealed abstract class ParserInput {
  def charAt(ix: Int): Char
  def length: Int
  def sliceString(start: Int, end: Int): String
  override def toString: String = sliceString(0, length)

  /**
   * Gets the input line with the given number as a String.
   * Note: the first line is line number one!
   */
  def getLine(line: Int): String = toString.split('\n')(line - 1)
}

object ParserInput {
  val Empty = apply(Array.empty[Byte])

  implicit def apply(bytes: Array[Byte]): ParserInput = apply(bytes, UTF8)

  def apply(bytes: Array[Byte], charset: Charset): ParserInput =
    new ParserInput {
      def charAt(ix: Int) = bytes(ix).toChar
      def length = bytes.length
      def sliceString(start: Int, end: Int) = new String(bytes, start, end - start, charset)
    }

  implicit def apply(string: String): ParserInput =
    new ParserInput {
      def charAt(ix: Int) = string.charAt(ix)
      def length = string.length
      def sliceString(start: Int, end: Int) = string.substring(start, end)
    }

  implicit def apply(chars: Array[Char]): ParserInput = apply(new String(chars))
}