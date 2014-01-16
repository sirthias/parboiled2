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
import scala.annotation.tailrec

trait ParserInput {
  /**
   * Returns the character at the given (zero-based) index.
   * Note: this method is hot and should be small and efficient.
   * A range-check is not required for the parser to work correctly.
   */
  def charAt(ix: Int): Char

  /**
   * The number of characters in this input.
   * Note: this method is hot and should be small and efficient.
   */
  def length: Int

  /**
   * Returns the characters between index `start` (inclusively) and `end` (exclusively) as a `String`.
   */
  def sliceString(start: Int, end: Int): String

  /**
   * Gets the input line with the given number as a String.
   * Note: the first line is line number one!
   */
  def getLine(line: Int): String
}

object ParserInput {
  val Empty = apply(Array.empty[Byte])

  implicit def apply(bytes: Array[Byte]): ByteArrayBasedParser = apply(bytes, UTF8)
  implicit def apply(string: String): StringBasedParser = new StringBasedParser(string)
  implicit def apply(chars: Array[Char]): CharArrayBasedParser = new CharArrayBasedParser(chars)
  def apply(bytes: Array[Byte], charset: Charset): ByteArrayBasedParser = new ByteArrayBasedParser(bytes, charset)

  abstract class DefaultParserInput extends ParserInput {
    def getLine(line: Int): String = {
      @tailrec def rec(ix: Int, lineStartIx: Int, lineNr: Int): String =
        if (ix < length)
          if (charAt(ix) == '\n')
            if (lineNr < line) rec(ix + 1, ix + 1, lineNr + 1)
            else sliceString(lineStartIx, ix)
          else rec(ix + 1, lineStartIx, lineNr)
        else if (lineNr == line) sliceString(lineStartIx, ix) else ""
      rec(ix = 0, lineStartIx = 0, lineNr = 1)
    }
  }

  class ByteArrayBasedParser(bytes: Array[Byte], charset: Charset) extends DefaultParserInput {
    def charAt(ix: Int) = bytes(ix).toChar
    def length = bytes.length
    def sliceString(start: Int, end: Int) = new String(bytes, start, end - start, charset)
  }

  class StringBasedParser(string: String) extends DefaultParserInput {
    def charAt(ix: Int) = string.charAt(ix)
    def length = string.length
    def sliceString(start: Int, end: Int) = string.substring(start, end)
  }

  class CharArrayBasedParser(chars: Array[Char]) extends DefaultParserInput {
    def charAt(ix: Int) = chars(ix)
    def length = chars.length
    def sliceString(start: Int, end: Int) = new String(chars, start, end - start)
  }
}