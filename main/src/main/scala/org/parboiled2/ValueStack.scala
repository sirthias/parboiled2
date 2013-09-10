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

import shapeless._
import scala.annotation.tailrec

private[parboiled2] class ValueStack {
  import ValueStack._

  private[this] var buffer = new Array[Any](16)
  var top: Int = 0 // index of the next value that is pushed

  def clear(): Unit = top = 0

  def push(value: Any): Unit = {
    ensureSize(top)
    buffer(top) = value
    top += 1
  }

  def pop(): Any =
    if (top > 0) {
      top -= 1
      buffer(top)
    } else sys.error("ValueStack underflow")

  def popHList[L <: HList](start: Int = 0, end: Int = top, prependTo: HList = HNil): L = {
    top = start
    toHList[L](start, end, prependTo)
  }

  @tailrec
  final def toHList[L <: HList](start: Int = 0, end: Int = top, prependTo: HList = HNil): L =
    if (start == end) prependTo.asInstanceOf[L]
    else toHList[L](start + 1, end, ::(buffer(start), prependTo))

  private def ensureSize(size: Int): Unit =
    if (buffer.length < size)
      if (size <= MaxSize) {
        val newSize = math.min(math.max(buffer.length * 2, size), MaxSize)
        val newBuffer = new Array[Any](newSize)
        System.arraycopy(buffer, 0, newBuffer, 0, buffer.length)
        buffer = newBuffer
      } else sys.error("ValueStack overflow")

  override def toString() = s"VStck($top - ${buffer mkString " | "})"
}

private[parboiled2] object ValueStack {
  final val MaxSize = 1024
}
