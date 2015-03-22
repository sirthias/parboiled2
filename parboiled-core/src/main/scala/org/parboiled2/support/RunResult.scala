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
sealed trait RunResult[Ctx, T] {
  type Out <: Rule[_, _, _]
}

object RunResult {
  implicit def fromAux[Ctx, T, Out0 <: Rule[_, _, _]](implicit aux: Aux[Ctx, T, Out0]): RunResult[Ctx, T] { type Out = Out0 } = `n/a`

  sealed trait Aux[Ctx, T, Out <: Rule[_, _, _]]

  object Aux extends Aux1 {
    implicit def forRule[Ctx, C <: Ctx, I <: HList, O <: HList]: Aux[Ctx, Rule[C, I, O], Rule[C, I, O]] = `n/a`
    //implicit def forFHList[I <: HList, R, In0 <: HList, Out0 <: HList](implicit x: JA[I, R, In0, Out0]): Aux[I ⇒ R, Rule[In0, Out0]] = `n/a`
  }

  abstract class Aux1 extends Aux2 {
    implicit def forF1[C, Z, R, Out <: Rule[_, _, _]](
      implicit x: JA[C, Z :: HNil, R, Out]): Aux[C, Z ⇒ R, Out] = `n/a`

    implicit def forF2[C, Y, Z, R, Out <: Rule[_, _, _]](
      implicit x: JA[C, Y :: Z :: HNil, R, Out]): Aux[C, (Y, Z) ⇒ R, Out] = `n/a`

    implicit def forF3[C, X, Y, Z, R, Out <: Rule[_, _, _]](
      implicit x: JA[C, X :: Y :: Z :: HNil, R, Out]): Aux[C, (X, Y, Z) ⇒ R, Out] = `n/a`

    implicit def forF4[C, W, X, Y, Z, R, Out <: Rule[_, _, _]](
      implicit x: JA[C, W :: X :: Y :: Z :: HNil, R, Out]): Aux[C, (W, X, Y, Z) ⇒ R, Out] = `n/a`

    implicit def forF5[C, V, W, X, Y, Z, R, Out <: Rule[_, _, _]](
      implicit x: JA[C, V :: W :: X :: Y :: Z :: HNil, R, Out]): Aux[C, (V, W, X, Y, Z) ⇒ R, Out] = `n/a`
  }

  abstract class Aux2 {
    protected type JA[C, I <: HList, R, Out <: Rule[_, _, _]] = Join.Aux[C, I, HNil, R, HNil, Out]
    implicit def forAny[C, T]: Aux[C, T, Rule[C, HNil, HNil]] = `n/a`
  }
}