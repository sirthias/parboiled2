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

abstract class RuleDSL {

  implicit def ch(c: Char): Rule0 = Rule.notAvailableAtRuntime
  implicit def str(s: String): Rule0 = Rule.notAvailableAtRuntime
  def optional[L <: HList](r: Rule[L])(implicit u: Optionalizer[L]): Rule[u.Out] = Rule.notAvailableAtRuntime
  def zeroOrMore[L <: HList](r: Rule[L])(implicit u: Sequencer[L]): Rule[u.Out] = Rule.notAvailableAtRuntime
  def oneOrMore[L <: HList](r: Rule[L])(implicit u: Sequencer[L]): Rule[u.Out] = Rule.notAvailableAtRuntime
  def &(r: Rule[_]): Rule0 = Rule.notAvailableAtRuntime

  def EOI = org.parboiled2.EOI

  implicit class PimpedString(underlying: String) {
    def -(other: String): Rule0 = Rule.notAvailableAtRuntime
  }
}

trait Prepender[A <: HList, B <: HList] {
  type Out <: HList
}
object Prepender {
  implicit def hnilPrepend[A <: HList, B <: HNil] =
    new Prepender[A, B] {
      type Out = A
    }

  implicit def apply[A <: HList, B <: HList, R <: HList](implicit prepend: PrependAux[A, B, R]) =
    new Prepender[A, B] {
      type Out = R
    }
}

trait Optionalizer[L <: HList] {
  type Out <: HList
}
object Optionalizer extends LowerPriorityOptionalizer {
  implicit def forHNil[L <: HNil] = new Optionalizer[L] {
    type Out = HNil
  }
  implicit def forSingleElemHList[T] = new Optionalizer[T :: HNil] {
    type Out = Option[T] :: HNil
  }
}
private[parboiled2] abstract class LowerPriorityOptionalizer {
  implicit def default[L <: HList] = new Optionalizer[L] {
    type Out = L
  }
}

trait Sequencer[L <: HList] {
  type Out <: HList
}
object Sequencer extends LowerPrioritySequenceBuilder {
  implicit def forHNil[L <: HNil] = new Sequencer[L] {
    type Out = HNil
  }
  implicit def forSingleElemHList[T] = new Sequencer[T :: HNil] {
    type Out = Seq[T] :: HNil
  }
}
private[parboiled2] abstract class LowerPrioritySequenceBuilder {
  implicit def default[L <: HList] = new Sequencer[L] {
    type Out = Seq[L] :: HNil
  }
}