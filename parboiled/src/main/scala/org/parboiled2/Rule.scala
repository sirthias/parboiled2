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
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.internal.annotations.compileTimeOnly
import shapeless.ops.hlist.{ Prepend, ReversePrepend }

sealed trait RuleX {
  def matched: Boolean
}

/**
 * The general model of a parser rule.
 * It is characterized by consuming a certain number of elements from the value stack (whose types are captured by the
 * HList type parameter `I` for "Input") and itself pushing a certain number of elements onto the value stack (whose
 * types are captured by the HList type parameter `O` for "Output").
 *
 * At runtime there are only two instances of this class which signal whether the rule has matched (or mismatched)
 * at the current point in the input.
 */
sealed class Rule[-I <: HList, +O <: HList](val matched: Boolean) extends RuleX {
  // TODO: model as value class
  // This would give us a performance benefit since we'd save the heap access to the `matched` member
  // However, https://issues.scala-lang.org/browse/SI-6260 is quite a serious problem for this design,
  // so until this issue is fixed we better stick to this non-value-class-based model

  // general concatenation of two rules,
  // e.g. (using an abbreviated HList notation):
  //   Rule[, A] ~ Rule[, B] = Rule[, A:B]
  //   Rule[A:B:C, D:E:F] ~ Rule[F, G:H] = Rule[A:B:C, D:E:G:H]
  //   Rule[A, B:C] ~ Rule[D:B:C, E:F] = Rule[D:A, E:F]
  @compileTimeOnly("Calls to `~` must be inside `rule` macro")
  def ~[I2 <: HList, O2 <: HList](that: Rule[I2, O2])(implicit i: TailSwitch[I2, O @uncheckedVariance, I @uncheckedVariance],
                                                      o: TailSwitch[O @uncheckedVariance, I2, O2]): Rule[i.Out, o.Out] = `n/a`

  @compileTimeOnly("Calls to `|` must be inside `rule` macro")
  def |[I2 <: I, O2 >: O <: HList](that: Rule[I2, O2]): Rule[I2, O2] = `n/a`

  @compileTimeOnly("Calls to `unary_!` must be inside `rule` macro")
  def unary_!(): Rule0 = `n/a`
}

/**
 * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
 */
object Rule {
  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  val Matched = new Rule0(true)

  /**
   * THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
   */
  val Mismatched = new Rule0(false)

  implicit class Runnable[L <: HList](rule: RuleN[L]) {
    def apply(): Parser.Result[L] = macro Parser.runImpl[L]
  }
}

abstract class RuleDSL
  extends RuleDSLBasics
  with RuleDSLCombinators
  with RuleDSLActions

//////////////////////////////// SUPPORTING TYPE-CLASSES ////////////////////////////////////

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