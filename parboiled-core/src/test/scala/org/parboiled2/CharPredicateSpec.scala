/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2

import utest._

object CharPredicateSpec extends TestSuite {

  val tests = Tests {
    "CharPredicates" - {

      "correctly mask characters" - {
        inspectMask(CharPredicate("4")) ==> "0010000000000000|0000000000000000"
        inspectMask(CharPredicate("a")) ==> "0000000000000000|0000000200000000"
        CharPredicate("&048z{~").toString ==> "CharPredicate.MaskBased(&048z{~)"
        show(CharPredicate("&048z{~")) ==> "&048z{~"
      }

      "support `testAny`" - {
        assert(
          CharPredicate("abc").matchesAny("0125!") == false,
          CharPredicate("abc").matchesAny("012c5!")
        )
      }

      "support `indexOfFirstMatch`" - {
        CharPredicate("abc").indexOfFirstMatch("0125!") ==> -1
        CharPredicate("abc").indexOfFirstMatch("012c5!") ==> 3
      }

      "correctly support non-masked content" - {
        val colonSlashEOI = CharPredicate(':', '/', EOI)
        assert(
          colonSlashEOI(':'),
          colonSlashEOI('/'),
          colonSlashEOI(EOI),
          colonSlashEOI('x') == false
        )
      }

      "be backed by a mask where possible" - {
        CharPredicate('1' to '9').toString ==> "CharPredicate.MaskBased(123456789)"
        (CharPredicate('1' to '3') ++ CharPredicate('5' to '8')).toString ==> "CharPredicate.MaskBased(1235678)"
        (CharPredicate('1' to '3') ++ "5678").toString ==> "CharPredicate.MaskBased(1235678)"
        (CharPredicate('1' to '6') -- CharPredicate('2' to '4')).toString ==> "CharPredicate.MaskBased(156)"
        (CharPredicate('1' to '6') -- "234").toString ==> "CharPredicate.MaskBased(156)"
      }
      "be backed by an array where possible" - {
        CharPredicate("abcäüö").toString ==> "CharPredicate.ArrayBased(abcäöü)"
        (CharPredicate("abcäüö") -- "äö").toString ==> "CharPredicate.ArrayBased(abcü)"
      }
      "be backed by a range where possible" - {
        CharPredicate(
          '1' to 'Ä'
        ).toString ==> "CharPredicate.RangeBased(start = 1, end = Ä, step = 1, inclusive = true)"
      }
    }

    def show(pred: CharPredicate): String = {
      val chars = ('\u0000' to '\u0080').flatMap(c => Some(c) filter pred).toArray
      new String(chars)
    }

    def inspectMask(pred: CharPredicate) = {
      val (lowMask, highMask) = pred match {
        case CharPredicate.MaskBased(a, b) => a -> b
        case _                             => throw new IllegalStateException()
      }
      "%016x|%016x".format(lowMask, highMask)
    }
  }
}
