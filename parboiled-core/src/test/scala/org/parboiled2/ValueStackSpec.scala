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

import shapeless.{HList, HNil}
import utest.{intercept, _}

object ValueStackSpec extends TestSuite {

  val tests = Tests {

    "The ValueStack should properly support" - {

      "push, size, toList" - new TestStack(stackSize = 3) {
        size ==> 0
        push(42)
        size ==> 1
        toList ==> List(42)
        push("yes")
        push(3.0)
        size ==> 3
        toList ==> List(42, "yes", 3.0)
        intercept[ValueStackOverflowException] {
          push("overflow")
        }
      }

      "pushAll, toHList" - new TestStack(stackSize = 3) {
        pushAll(42 :: "yes" :: 4.5 :: HNil)
        size ==> 3
        toHList[HList]() ==> 42 :: "yes" :: 4.5 :: HNil
        intercept[ValueStackOverflowException] {
          pushAll("overflow" :: HNil)
        }
        intercept[IllegalArgumentException] {
          toHList[HList](start = -1)
        }
        intercept[IllegalArgumentException] {
          toHList[HList](start = 1, end = 0)
        }
      }

      "insert" - new TestStack(stackSize = 4) {
        pushAll(1 :: 2 :: 3 :: HNil)
        insert(2, 1.5)
        toList ==> List(1, 1.5, 2, 3)
        intercept[IllegalArgumentException] {
          insert(-1, 0)
        }
        intercept[ValueStackOverflowException] {
          insert(2, 0)
        }
        intercept[ValueStackUnderflowException] {
          insert(5, 0)
        }
      }

      "pop" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: HNil)
        pop() ==> 3
        toList ==> List(1, 2)
        pop() ==> 2
        toList ==> List(1)
        pop() ==> 1
        assert(isEmpty)
        intercept[ValueStackUnderflowException] {
          pop()
        }
      }

      "pullOut" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: 4 :: HNil)
        pullOut(1) ==> 3
        toList ==> List(1, 2, 4)
        pullOut(2) ==> 1
        toList ==> List(2, 4)
        intercept[ValueStackUnderflowException] {
          pullOut(2)
        }
        intercept[IllegalArgumentException] {
          pullOut(-1)
        }
      }

      "peek" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: HNil)
        peek ==> 3
        peek(1) ==> 2
        peek(2) ==> 1
        intercept[ValueStackUnderflowException] {
          peek(3)
        }
        intercept[IllegalArgumentException] {
          pullOut(-1)
        }
      }

      "poke" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: HNil)
        poke(0, "3")
        toList ==> List(1, 2, "3")
        poke(1, "2")
        toList ==> List(1, "2", "3")
        poke(2, "1")
        toList ==> List("1", "2", "3")
        intercept[ValueStackUnderflowException] {
          poke(3, 0)
        }
        intercept[IllegalArgumentException] {
          poke(-1, 0)
        }
      }

      "swap" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: HNil)
        swap()
        toList ==> List(1, 3, 2)
        pop()
        pop()
        intercept[ValueStackUnderflowException] {
          swap()
        }
      }

      "swap3" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: HNil)
        swap3()
        toList ==> List(3, 2, 1)
        pop()
        intercept[ValueStackUnderflowException] {
          swap3()
        }
      }

      "swap4" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: 4 :: HNil)
        swap4()
        toList ==> List(4, 3, 2, 1)
        pop()
        intercept[ValueStackUnderflowException] {
          swap4()
        }
      }

      "swap5" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: 4 :: 5 :: HNil)
        swap5()
        toList ==> List(5, 4, 3, 2, 1)
        pop()
        intercept[ValueStackUnderflowException] {
          swap5()
        }
      }

    }
  }

  class TestStack(stackSize: Int) extends ValueStack(stackSize, stackSize)
}
