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

import org.parboiled2.util.Base64

/**
 * Rules for parsing of RFC2045 Base-64 encoded strings.
 */
object Rfc2045Parsing extends Base64Parsing(Base64.rfc2045())

/**
 * Rules for general parsing of Base-64 encoded strings.
 */
class Base64Parsing(val codec: Base64) extends SimpleParser {

  def this(alphabet: String) = this(new Base64(alphabet))

  private val Alphabet = (CharPredicate(codec.getAlphabet) ++ codec.fillChar).asMaskBased
  private val AlphabetPlusNewlines = (Alphabet ++ '\n').asMaskBased

  private type Decoder = Array[Char] ⇒ Array[Byte]
  private val stringDecoder: Decoder = codec.decodeFast
  private val blockDecoder: Decoder = codec.decode

  private val Base64StringOrBlock = rule[CharPredicate.MaskBased, Decoder]() { (pred, decoder) ⇒
    val start = state.cursor
    oneOrMore(pred) ~ run {
      decoder(state.input.sliceCharArray(start, state.cursor)) match {
        case null  ⇒ MISMATCH
        case bytes ⇒ push(bytes)
      }
    }
  }

  /**
   * Parses a Base64-encoded string and decodes it onto the value stack.
   */
  val Base64String: Rule1[Array[Byte]] = rule { Base64StringOrBlock(Alphabet, stringDecoder) }

  /**
   * Parses a Base64-encoded string potentially containing newlines and decodes it onto the value stack.
   */
  val Base64Block: Rule1[Array[Byte]] = rule { Base64StringOrBlock(AlphabetPlusNewlines, blockDecoder) }
}