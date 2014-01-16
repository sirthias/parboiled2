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

import shapeless._
import org.parboiled2._

// phantom type, only used for rule DSL typing
sealed trait RunResult[T] {
  type Out <: RuleX
}

object RunResult extends RunResult1 {
  implicit def forRule[R <: RuleX]: RunResult[R] { type Out = R } = `n/a`
  //implicit def forFHList[I <: HList, R, In0 <: HList, Out0 <: HList](implicit x: JA[I, R, In0, Out0]): RunResult[I ⇒ R] { type Out = Rule[In0, Out0] } = `n/a`
}

abstract class RunResult1 extends RunResult2 {
  protected type JA[I <: HList, R, In0 <: HList, Out0 <: HList] = Join.Aux[I, HNil, HNil, R, HNil, In0, Out0]

  implicit def forF1[Z, R, In0 <: HList, Out0 <: HList](implicit x: JA[Z :: HNil, R, In0, Out0]): RunResult[Z ⇒ R] { type Out = Rule[In0, Out0] } = `n/a`
  implicit def forF2[Y, Z, R, In0 <: HList, Out0 <: HList](implicit x: JA[Y :: Z :: HNil, R, In0, Out0]): RunResult[(Y, Z) ⇒ R] { type Out = Rule[In0, Out0] } = `n/a`
  implicit def forF3[X, Y, Z, R, In0 <: HList, Out0 <: HList](implicit x: JA[X :: Y :: Z :: HNil, R, In0, Out0]): RunResult[(X, Y, Z) ⇒ R] { type Out = Rule[In0, Out0] } = `n/a`
  implicit def forF4[W, X, Y, Z, R, In0 <: HList, Out0 <: HList](implicit x: JA[W :: X :: Y :: Z :: HNil, R, In0, Out0]): RunResult[(W, X, Y, Z) ⇒ R] { type Out = Rule[In0, Out0] } = `n/a`
  implicit def forF5[V, W, X, Y, Z, R, In0 <: HList, Out0 <: HList](implicit x: JA[V :: W :: X :: Y :: Z :: HNil, R, In0, Out0]): RunResult[(V, W, X, Y, Z) ⇒ R] { type Out = Rule[In0, Out0] } = `n/a`
}

abstract class RunResult2 {
  implicit def forAny[T]: RunResult[T] { type Out = Rule0 } = `n/a`
}
