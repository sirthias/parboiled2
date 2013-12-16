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
import shapeless.ops.hlist.ReversePrepend

// format: OFF

/**
 * type-level implementation of this logic:
 *   Out =
 *     R                      if T has a tail of type L
 *     (L dropRight T) ::: R  if L has a tail of type T
 */
sealed trait TailSwitch[L <: HList, T <: HList, R <: HList] {
  type Out <: HList
}
object TailSwitch {
  implicit def tailSwitch[L <: HList, T <: HList, R <: HList, Out0 <: HList]
  (implicit ts: TailSwitch0[L, L, T, T, R, HNil, Out0]) =
    new TailSwitch[L, T, R] {
      type Out = Out0
    }
}

// type-level implementation of this algorithm:
//   @tailrec def rec(L, LI, T, TI, R, RI) =
//     if (TI <: L) R
//     else if (LI <: T) RI.reverse ::: R
//     else if (LI <: HNil) rec(L, HNil, T, TI.tail, R, RI)
//     else if (TI <: HNil) rec(L, LI.tail, T, HNil, R, LI.head :: RI)
//     rec(L, LI.tail, T, TI.tail, R, LI.head :: RI)
//   rec(L, L, T, T, R, HNil)
sealed trait TailSwitch0[L <: HList, LI <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList, Out <: HList]

object TailSwitch0 extends TailSwitch0_1 {
  // if TI <: L then Out = R
  implicit def terminate1[L <: HList, LI <: HList, T <: HList, TI <: L, R <: HList, RI <: HList]: TailSwitch0[L, LI, T, TI, R, RI, R] = `n/a`
}

private[parboiled2] abstract class TailSwitch0_1 extends TailSwitch0_2 {
  // if LI <: T then Out = RI.reverse ::: R
  implicit def terminate2[T <: HList, TI <: HList, L <: HList, LI <: T, R <: HList, RI <: HList, Out <: HList]
  (implicit rp: ReversePrepend.Aux[RI, R, Out]): TailSwitch0[L, LI, T, TI, R, RI, Out] = `n/a`
}

private[parboiled2] abstract class TailSwitch0_2 {
  implicit def iter1[L <: HList, T <: HList, TH, TT <: HList, R <: HList, RI <: HList, Out <: HList]
  (implicit next: TailSwitch0[L, HNil, T, TT, R, RI, Out]): TailSwitch0[L, HNil, T, TH :: TT, R, RI, Out] = `n/a`

  implicit def iter2[L <: HList, LH, LT <: HList, T <: HList, R <: HList, RI <: HList, Out <: HList]
  (implicit next: TailSwitch0[L, LT, T, HNil, R, LH :: RI, Out]): TailSwitch0[L, LH :: LT, T, HNil, R, RI, Out] = `n/a`

  implicit def iter3[L <: HList, LH, LT <: HList, T <: HList, TH, TT <: HList, R <: HList, RI <: HList, Out <: HList]
  (implicit next: TailSwitch0[L, LT, T, TT, R, LH :: RI, Out]): TailSwitch0[L, LH :: LT, T, TH :: TT, R, RI, Out] = `n/a`
}