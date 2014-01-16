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

package org.parboiled2.support

/**
 * A mutable untyped stack of values.
 * In most cases you won't have to access its API directly since parboiled2's DSL
 * should allow you a safer and easier way to interact with the stack.
 * However, in some cases, when you know what you are doing, direct access can be helpful.
 */
class SeqBuilder extends Iterable[Any] {
  private[this] var array: Array[Any] = org.parboiled2.EmptyArray
  private[this] var _size = 0

  override def size: Int = _size
  override def isEmpty: Boolean = _size == 0
  def clear(): Unit = _size = 0

  def +=(value: Any): Unit = {
    val oldSize = _size
    val newSize = oldSize + 1
    ensureSize(newSize)
    array(oldSize) = value
    _size = newSize
  }

  /**
   * Returns all current stack elements as a new array.
   */
  def toArray: Array[Any] = {
    val a = new Array[Any](_size)
    System.arraycopy(array, 0, a, 0, _size)
    a
  }

  /**
   * Returns all current stack elements as a new array.
   */
  def result(): Seq[Any] = collection.mutable.WrappedArray.make(toArray)

  /**
   * Returns an iterator that iterates over a *snapshot* of the stack elements
   * at the time of this method call. I.e. subsequent mutations are not visible
   * to the iterator.
   */
  def iterator: Iterator[Any] = toArray.iterator

  private def ensureSize(requiredSize: Int): Unit =
    if (array.length < requiredSize) {
      val newSize = math.max(math.max(array.length * 2, requiredSize), 16)
      val newArray = new Array[Any](newSize)
      System.arraycopy(array, 0, newArray, 0, _size)
      array = newArray
    }
}