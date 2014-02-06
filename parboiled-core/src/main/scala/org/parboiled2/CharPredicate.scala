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

import scala.annotation.tailrec

sealed abstract class CharPredicate extends (Char ⇒ Boolean) {
  import CharPredicate._

  /**
   * determines wether this CharPredicate is an instance of the high-performance,
   * constant-time `CharMask` implementation.
   */
  def isCharMask: Boolean = this.isInstanceOf[CharMask]

  def asCharMask: CharMask =
    this match {
      case x: CharMask ⇒ x
      case _           ⇒ sys.error("CharPredicate is not a CharMask")
    }

  def ++(that: CharPredicate): CharPredicate
  def ++(chars: Seq[Char]): CharPredicate
  def --(that: CharPredicate): CharPredicate
  def --(chars: Seq[Char]): CharPredicate

  def ++(char: Char): CharPredicate = this ++ (char :: Nil)
  def --(char: Char): CharPredicate = this -- (char :: Nil)
  def ++(chars: String): CharPredicate = this ++ chars.toCharArray
  def --(chars: String): CharPredicate = this -- chars.toCharArray

  def intersect(that: CharPredicate): CharPredicate

  def negated: CharPredicate = this match {
    case Empty ⇒ All
    case All   ⇒ Empty
    case x     ⇒ from(c ⇒ !x(c))
  }

  def matchesAny(string: String): Boolean = {
    @tailrec def rec(ix: Int): Boolean =
      if (ix == string.length) false else if (this(string charAt ix)) true else rec(ix + 1)
    rec(0)
  }

  def matchesAll(string: String): Boolean = {
    @tailrec def rec(ix: Int): Boolean =
      if (ix == string.length) true else if (!this(string charAt ix)) false else rec(ix + 1)
    rec(0)
  }

  def indexOfFirstMatch(string: String): Int = {
    @tailrec def rec(ix: Int): Int =
      if (ix == string.length) -1 else if (this(string charAt ix)) ix else rec(ix + 1)
    rec(0)
  }

  def indexOfFirstMismatch(string: String): Int = {
    @tailrec def rec(ix: Int): Int =
      if (ix == string.length) -1 else if (this(string charAt ix)) rec(ix + 1) else ix
    rec(0)
  }

  def firstMatch(string: String): Option[Char] =
    indexOfFirstMatch(string) match {
      case -1 ⇒ None
      case ix ⇒ Some(string charAt ix)
    }

  def firstMismatch(string: String): Option[Char] =
    indexOfFirstMismatch(string) match {
      case -1 ⇒ None
      case ix ⇒ Some(string charAt ix)
    }

  private def or(that: Char ⇒ Boolean): CharPredicate =
    if (this == Empty) CharPredicate(that) else from(c ⇒ this(c) || that(c))
  private def and(that: Char ⇒ Boolean) =
    if (this == Empty) Empty else from(c ⇒ this(c) && that(c))
  private def andNot(that: Char ⇒ Boolean) =
    if (this == Empty) from(!that(_)) else from(c ⇒ this(c) && !that(c))
}

object CharPredicate {
  val Empty: CharPredicate = CharMask(0L, 0L)
  val All: CharPredicate = from(_ ⇒ true)
  val LowerAlpha = CharPredicate('a' to 'z')
  val UpperAlpha = CharPredicate('A' to 'Z')
  val Alpha = LowerAlpha ++ UpperAlpha
  val Digit = CharPredicate('0' to '9')
  val Digit19 = CharPredicate('1' to '9')
  val AlphaNum = Alpha ++ Digit
  val LowerHexLetter = CharPredicate('a' to 'f')
  val UpperHexLetter = CharPredicate('A' to 'F')
  val HexLetter = LowerHexLetter ++ UpperHexLetter
  val HexDigit = Digit ++ HexLetter
  val Visible = CharPredicate('\u0021' to '\u007e')
  val Printable = Visible ++ ' '

  def from(predicate: Char ⇒ Boolean): CharPredicate =
    predicate match {
      case x: CharPredicate ⇒ x
      case x                ⇒ GeneralCharPredicate(x)
    }

  def apply(magnets: ApplyMagnet*): CharPredicate = (Empty /: magnets) { (a, m) ⇒ a ++ m.predicate }

  class ApplyMagnet(val predicate: CharPredicate)
  object ApplyMagnet {
    implicit def fromPredicate(predicate: Char ⇒ Boolean): ApplyMagnet = new ApplyMagnet(from(predicate))
    implicit def fromChar(c: Char): ApplyMagnet = fromChars(c :: Nil)
    implicit def fromCharArray(array: Array[Char]): ApplyMagnet = fromChars(array)
    implicit def fromString(chars: String): ApplyMagnet = fromChars(chars)
    implicit def fromChars(chars: Seq[Char]): ApplyMagnet = {
      @tailrec def rec(ix: Int, result: CharPredicate): CharPredicate =
        if (ix == chars.length) result else rec(ix + 1, result ++ chars(ix))
      new ApplyMagnet(rec(0, Empty))
    }
  }

  ///////////////////////// PRIVATE ////////////////////////////

  private def unmaskable(c: Char) = c >= 128

  // efficient handling of 7bit-ASCII chars
  case class CharMask private[CharPredicate] (lowMask: Long, highMask: Long) extends CharPredicate {
    def apply(c: Char): Boolean = {
      val mask = if (c < 64) lowMask else highMask
      ((1L << c) & ((c - 128) >> 31) & mask) != 0L // branchless for `(c < 128) && (mask & (1L << c) != 0)`
    }

    def ++(that: CharPredicate): CharPredicate = that match {
      case Empty               ⇒ this
      case _ if this == Empty  ⇒ that
      case CharMask(low, high) ⇒ CharMask(lowMask | low, highMask | high)
      case _                   ⇒ this or that
    }

    def ++(chars: Seq[Char]): CharPredicate = chars.foldLeft(this: CharPredicate) {
      case (_: CharMask, c) if unmaskable(c)  ⇒ this or new ArrayBasedPredicate(chars.toArray)
      case (CharMask(low, high), c) if c < 64 ⇒ CharMask(low | 1L << c, high)
      case (CharMask(low, high), c)           ⇒ CharMask(low, high | 1L << c)
      case (x, _)                             ⇒ x // once the fold acc is not a CharMask we are done
    }

    def --(that: CharPredicate): CharPredicate = that match {
      case Empty               ⇒ this
      case _ if this == Empty  ⇒ this
      case CharMask(low, high) ⇒ CharMask(lowMask & ~low, highMask & ~high)
      case _                   ⇒ this andNot that
    }

    def --(chars: Seq[Char]): CharPredicate =
      if (this != Empty) {
        chars.foldLeft(this: CharPredicate) {
          case (_: CharMask, c) if unmaskable(c)  ⇒ this andNot new ArrayBasedPredicate(chars.toArray)
          case (CharMask(low, high), c) if c < 64 ⇒ CharMask(low & ~(1L << c), high)
          case (CharMask(low, high), c)           ⇒ CharMask(low, high & ~(1L << c))
          case (x, _)                             ⇒ x // once the fold acc is not a CharMask we are done
        }
      } else this

    def intersect(that: CharPredicate) = that match {
      case Empty               ⇒ Empty
      case _ if this == Empty  ⇒ Empty
      case CharMask(low, high) ⇒ CharMask(lowMask & low, highMask & high)
      case _                   ⇒ this and that
    }

    override def toString(): String = "CharMask(%016x|%016x)" format (lowMask, highMask)
  }

  class ArrayBasedPredicate private[CharPredicate] (private val chars: Array[Char]) extends CharPredicate {
    import java.util.Arrays._
    sort(chars)

    def apply(c: Char): Boolean = binarySearch(chars, c) >= 0

    def ++(that: CharPredicate) = that match {
      case Empty                  ⇒ this
      case x: ArrayBasedPredicate ⇒ this ++ x.chars
      case _                      ⇒ this or that
    }

    def ++(other: Seq[Char]) =
      if (other.nonEmpty) new ArrayBasedPredicate((this -- other).chars ++ other.toArray[Char])
      else this

    def --(that: CharPredicate) = that match {
      case Empty                  ⇒ this
      case x: ArrayBasedPredicate ⇒ this -- x.chars
      case _                      ⇒ this andNot that
    }

    def --(other: Seq[Char]) =
      if (other.nonEmpty) {
        val otherChars = other.toArray
        new ArrayBasedPredicate(chars.filter(binarySearch(otherChars, _) < 0))
      } else this

    def intersect(that: CharPredicate) = that match {
      case Empty                  ⇒ Empty
      case x: ArrayBasedPredicate ⇒ new ArrayBasedPredicate(chars.intersect(x.chars))
      case _                      ⇒ this and that
    }
  }

  case class GeneralCharPredicate private[CharPredicate] (predicate: Char ⇒ Boolean) extends CharPredicate {
    def apply(c: Char) = predicate(c)

    def ++(that: CharPredicate): CharPredicate = that match {
      case Empty                               ⇒ this
      case GeneralCharPredicate(thatPredicate) ⇒ from(c ⇒ predicate(c) || thatPredicate(c))
      case _                                   ⇒ from(c ⇒ predicate(c) || that(c))
    }

    def ++(chars: Seq[Char]): CharPredicate =
      if (chars.nonEmpty) {
        val abp = new ArrayBasedPredicate(chars.toArray)
        from(c ⇒ predicate(c) || abp(c))
      } else this

    def --(that: CharPredicate): CharPredicate = that match {
      case Empty                               ⇒ this
      case GeneralCharPredicate(thatPredicate) ⇒ from(c ⇒ predicate(c) && !thatPredicate(c))
      case _                                   ⇒ from(c ⇒ predicate(c) && !that(c))
    }

    def --(chars: Seq[Char]): CharPredicate =
      if (chars.nonEmpty) {
        val abp = new ArrayBasedPredicate(chars.toArray)
        from(c ⇒ predicate(c) && !abp(c))
      } else this

    def intersect(that: CharPredicate) = that match {
      case Empty                               ⇒ Empty
      case GeneralCharPredicate(thatPredicate) ⇒ from(c ⇒ predicate(c) && that(c))
      case _                                   ⇒ this and that
    }
  }
}
