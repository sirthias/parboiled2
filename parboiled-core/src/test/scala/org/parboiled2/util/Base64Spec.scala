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

package org.parboiled2.util

import utest._
import java.nio.charset.StandardCharsets

object Base64Spec extends TestSuite {

  private val testVectors = Map(
    ""                      -> "",
    "f"                     -> "Zg==",
    "fo"                    -> "Zm8=",
    "foo"                   -> "Zm9v",
    "foob"                  -> "Zm9vYg==",
    "fooba"                 -> "Zm9vYmE=",
    "foobar"                -> "Zm9vYmFy",
    "@@ Hello @@ world @@!" -> "QEAgSGVsbG8gQEAgd29ybGQgQEAh"
  )

  val tests = Tests {
    "Base64" - {
      testVectors.foreach { case (expectedDecoded, expectedEncoded) =>
        val expectedDecodedBytes = expectedDecoded.getBytes(StandardCharsets.UTF_8)

        val encoded = Base64.rfc2045().encodeToString(expectedDecodedBytes, lineSep = false)

        assert(
          expectedEncoded == encoded,
          expectedDecodedBytes sameElements Base64.rfc2045().decode(encoded.toCharArray),
          expectedDecodedBytes sameElements Base64.rfc2045().decodeFast(encoded.toCharArray)
        )
      }
    }
  }
}
