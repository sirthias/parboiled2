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

import CharUtils.escape
import scala.util.{ Failure, Success, Try }
import shapeless.{ HNil, HList }

case class ParseError(position: Position, traces: Seq[RuleTrace]) extends RuntimeException

object ParseError {
  trait Strategy[L <: HList] {
    type Result
    def success(result: L): Result
    def parseError(error: ParseError): Result
    def failure(error: Throwable): Result
  }
  object Strategy extends AlternativeStrategies {
    implicit def Try[L <: HList, Out0](implicit unpack: Unpack.Aux[L, Out0]) =
      new Strategy[L] {
        type Result = Try[Out0]
        def success(result: L) = Success(unpack(result))
        def parseError(error: ParseError) = Failure(error)
        def failure(error: Throwable) = Failure(error)
      }
  }
  sealed abstract class AlternativeStrategies {
    implicit def Either[L <: HList, Out0](implicit unpack: Unpack.Aux[L, Out0]) =
      new Strategy[L] {
        type Result = Either[ParseError, Out0]
        def success(result: L) = Right(unpack(result))
        def parseError(error: ParseError) = Left(error)
        def failure(error: Throwable) = throw error
      }
    implicit def Throw[L <: HList, Out0](implicit unpack: Unpack.Aux[L, Out0]) =
      new Strategy[L] {
        type Result = Out0
        def success(result: L) = unpack(result)
        def parseError(error: ParseError) = throw error
        def failure(error: Throwable) = throw error
      }
  }
}

case class Position(index: Int, line: Int, column: Int)

// outermost (i.e. highest-level) rule first
case class RuleTrace(frames: Seq[RuleFrame]) {
  def format: String =
    frames.size match {
      case 0 ⇒ "<empty>"
      case 1 ⇒ RuleFrame.format(frames.head)
      case _ ⇒
        // we don't want to show intermediate Sequence and RuleCall frames in the trace
        def show(frame: RuleFrame) = !(frame.isInstanceOf[RuleFrame.Sequence] || frame.isInstanceOf[RuleFrame.RuleCall])
        frames.init.filter(show).map(RuleFrame.format).mkString("", " / ", " / " + RuleFrame.format(frames.last))
    }

  def isNegated: Boolean = (frames.count(_.anon == RuleFrame.NotPredicate) & 0x01) > 0
}

sealed trait RuleFrame {
  def anon: RuleFrame.Anonymous
}

object RuleFrame {
  def apply(frame: Anonymous, name: String): RuleFrame =
    if (name.isEmpty) frame else Named(name, frame)

  case class Named(name: String, anon: Anonymous) extends RuleFrame

  sealed trait Anonymous extends RuleFrame {
    def anon: Anonymous = this
  }
  case class Sequence(subs: Int) extends Anonymous
  case class FirstOf(subs: Int) extends Anonymous
  case class CharMatch(char: Char) extends Anonymous
  case class StringMatch(string: String) extends Anonymous
  case class IgnoreCaseChar(char: Char) extends Anonymous
  case class IgnoreCaseString(string: String) extends Anonymous
  case class CharPredicateMatch(predicate: CharPredicate) extends Anonymous
  case class AnyOf(string: String) extends Anonymous
  case class Times(min: Int, max: Int) extends Anonymous
  case class RuleCall(callee: String) extends Anonymous
  case class CharRange(from: Char, to: Char) extends Anonymous
  case object ANY extends Anonymous
  case object Optional extends Anonymous
  case object ZeroOrMore extends Anonymous
  case object OneOrMore extends Anonymous
  case object AndPredicate extends Anonymous
  case object NotPredicate extends Anonymous
  case object SemanticPredicate extends Anonymous
  case object Capture extends Anonymous
  case object Run extends Anonymous
  case object Push extends Anonymous
  case object Action extends Anonymous

  def format(frame: RuleFrame): String =
    frame match {
      case Named(name, _)                ⇒ name
      case Sequence(_)                   ⇒ "~"
      case FirstOf(_)                    ⇒ "|"
      case CharMatch(c)                  ⇒ "'" + escape(c) + '\''
      case StringMatch(s)                ⇒ '"' + escape(s) + '"'
      case IgnoreCaseChar(c)             ⇒ "'" + escape(c) + '\''
      case IgnoreCaseString(s)           ⇒ '"' + escape(s) + '"'
      case CharPredicateMatch(_)         ⇒ "<anon predicate>"
      case RuleCall(callee)              ⇒ '(' + callee + ')'
      case AnyOf(s)                      ⇒ '[' + escape(s) + ']'
      case Times(_, _)                   ⇒ "times"
      case CharRange(from, to)           ⇒ s"'${escape(from)}'-'${escape(to)}'"
      case ANY                           ⇒ "ANY"
      case Optional                      ⇒ "optional"
      case ZeroOrMore                    ⇒ "zeroOrMore"
      case OneOrMore                     ⇒ "oneOrMore"
      case AndPredicate                  ⇒ "&"
      case NotPredicate                  ⇒ "!"
      case SemanticPredicate             ⇒ "test"
      case Capture | Run | Push | Action ⇒ frame.toString
    }
}

//////////////////////////////// SUPPORTING TYPE-CLASSES ////////////////////////////////////

/**
 * "Unpacks" an HList if it has only zero or one element(s).
 *   Out =
 *     Unit  if L == HNil
 *     T     if L == T :: HNil
 *     L     otherwise
 *
 *  You can `import Unpack.dontUnpack` if you'd like to circumvent this unpacking logic.
 */
sealed trait Unpack[L <: HList] {
  type Out
  def apply(hlist: L): Out
}
object Unpack extends AlternativeUnpacks {
  import shapeless.::

  implicit def fromAux[L <: HList, Out0](implicit aux: Aux[L, Out0]) = new Unpack[L] {
    type Out = Out0
    def apply(hlist: L) = aux(hlist)
  }

  sealed trait Aux[L <: HList, Out0] {
    def apply(hlist: L): Out0
  }

  object Aux {
    implicit object hnil extends Aux[HNil, Unit] {
      def apply(hlist: HNil): Unit = ()
    }
    private object SingleUnpack extends Aux[Any :: HList, Any] {
      def apply(hlist: Any :: HList): Any = hlist.head
    }
    implicit def single[T]: Aux[T :: HNil, T] = SingleUnpack.asInstanceOf[Aux[T :: HNil, T]]
  }
}

sealed abstract class AlternativeUnpacks {
  private object DontUnpack extends Unpack.Aux[HList, HList] {
    def apply(hlist: HList): HList = hlist
  }

  /**
   * Import if you'd like to *always* deliver the valueStack as an `HList`
   * at the end of the parsing run, even if it has only zero or one element(s).
   */
  implicit def dontUnpack[L <: HList]: Unpack.Aux[L, L] = DontUnpack.asInstanceOf[Unpack.Aux[L, L]]
}

