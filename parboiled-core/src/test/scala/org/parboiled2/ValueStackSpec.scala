package org.parboiled2

import shapeless.{HList, HNil}
import utest.{intercept, _}

object ValueStackSpec extends TestSuite {

  val tests = Tests{

    "The ValueStack should properly support" - {

      "push, size, toList" - new TestStack(stackSize = 3) {
        assert(size == 0)
        push(42)
        assert(size == 1)
        assert(toList == List(42))
        push("yes")
        push(3.0)
        assert(size == 3)
        assert(toList == List(42, "yes", 3.0))
        intercept[ValueStackOverflowException] {
          push("overflow")
        }
      }

      "pushAll, toHList" - new TestStack(stackSize = 3) {
        pushAll(42 :: "yes" :: 4.5 :: HNil)
        assert(size == 3)
        assert(toHList[HList]() == 42 :: "yes" :: 4.5 :: HNil)
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
        assert(toList == List(1, 1.5, 2, 3))
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
        assert(pop() == 3)
        assert(toList == List(1, 2))
        assert(pop() == 2)
        assert(toList == List(1))
        assert(pop() == 1)
        assert(isEmpty)
        intercept[ValueStackUnderflowException] {
          pop()
        }
      }

      "pullOut" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: 4 :: HNil)
        assert(pullOut(1) == 3)
        assert(toList == List(1, 2, 4))
        assert(pullOut(2) == 1)
        assert(toList == List(2, 4))
        intercept[ValueStackUnderflowException] {
          pullOut(2)
        }
        intercept[IllegalArgumentException] {
          pullOut(-1)
        }
      }

      "peek" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: HNil)
        assert(peek == 3)
        assert(peek(1) == 2)
        assert(peek(2) == 1)
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
        assert(toList == List(1, 2, "3"))
        poke(1, "2")
        assert(toList == List(1, "2", "3"))
        poke(2, "1")
        assert(toList == List("1", "2", "3"))
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
        assert(toList == List(1, 3, 2))
        pop()
        pop()
        intercept[ValueStackUnderflowException] {
          swap()
        }
      }

      "swap3" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: HNil)
        swap3()
        assert(toList == List(3, 2, 1))
        pop()
        intercept[ValueStackUnderflowException] {
          swap3()
        }
      }

      "swap4" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: 4 :: HNil)
        swap4()
        assert(toList == List(4, 3, 2, 1))
        pop()
        intercept[ValueStackUnderflowException] {
          swap4()
        }
      }

      "swap5" - new TestStack(stackSize = 8) {
        pushAll(1 :: 2 :: 3 :: 4 :: 5 :: HNil)
        swap5()
        assert(toList == List(5, 4, 3, 2, 1))
        pop()
        intercept[ValueStackUnderflowException] {
          swap5()
        }
      }

    }
  }

  class TestStack(stackSize: Int) extends ValueStack(stackSize, stackSize)
}
