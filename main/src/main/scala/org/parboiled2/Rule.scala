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

/**
 * The general model of a parser rule.
 * It is characterized by consuming a certain number of elements from the value stack (whose types are captured by the
 * HList type parameter `I` for "Input") and itself pushing a certain number of elements onto the value stack (whose
 * types are captured by the HList type parameter `O` for "Output").
 *
 * At runtime there are only two instances of this class which signal whether the rule has matched (or mismatched)
 * at the current point in the input.
 */
sealed abstract class Rule[-I <: HList, +O <: HList] {
  // TODO: model as value class wrapping a `matched: Boolean`
  // This would give us a (small) performance benefit since we'd save the comparison to the predefined
  // `Matched` and `Mismatched` objects.
  // However, https://issues.scala-lang.org/browse/SI-6260 is quite a serious problem for this design,
  // so until this issue is fixed we better stick to this non-value-class-based model

  import Rule.notAvailableAtRuntime

  // general concatenation of two rules,
  // e.g. (using an abbreviated HList notation):
  //   Rule[, A] ~ Rule[, B] = Rule[, A:B]
  //   Rule[A:B:C, D:E:F] ~ Rule[F, G:H] = Rule[A:B:C, D:E:G:H]
  //   Rule[A, B:C] ~ Rule[D:B:C, E:F] = Rule[D:A, E:F]
  def ~[I2 <: HList, O2 <: HList](that: Rule[I2, O2])(implicit i: TailSwitch[I2, O @uncheckedVariance, I @uncheckedVariance],
                                                      o: TailSwitch[O @uncheckedVariance, I2, O2]): Rule[i.Out, o.Out] = notAvailableAtRuntime

  def |[I2 <: I, O2 >: O <: HList](that: Rule[I2, O2]): Rule[I2, O2] = notAvailableAtRuntime

  def unary_!(): Rule0 = notAvailableAtRuntime

  def matched: Boolean = this eq Rule.Matched
  def mismatched: Boolean = this eq Rule.Mismatched
}

private[parboiled2] object Rule {
  class NotAvailableAtRuntimeException private[Rule] () extends RuntimeException

  def notAvailableAtRuntime: Nothing = throw new NotAvailableAtRuntimeException

  private object Matched extends Rule0
  private object Mismatched extends Rule0

  def apply[I <: HList, O <: HList](): Rule[I, O] = notAvailableAtRuntime

  def apply[I <: HList, O <: HList](m: Boolean): Rule[I, O] = if (m) matched else mismatched

  def matched[I <: HList, O <: HList]: Rule[I, O] = Matched.asInstanceOf[Rule[I, O]]
  def mismatched[I <: HList, O <: HList]: Rule[I, O] = Mismatched.asInstanceOf[Rule[I, O]]
}

abstract class RuleDSL {
  import Rule.notAvailableAtRuntime

  implicit def ch(c: Char): Rule0 = notAvailableAtRuntime
  implicit def str(s: String): Rule0 = notAvailableAtRuntime

  // runs its inner rule and succeeds even if the inner rule doesn't
  // Resulting rule type:
  //   if (inner rule == Rule0) Rule0
  //   if (inner rule == Rule1[T]) Rule1[Option[T]]
  //   if (inner rule == Rule[I, O <: I]) Rule[I, O] // so called "reduction", which leaves the value stack unchanged (on a type level)
  def optional[I <: HList, O <: HList](r: Rule[I, O])(implicit o: Optionalizer[I, O]): Rule[o.In, o.Out] = notAvailableAtRuntime

  // runs its inner rule until it fails, always succeeds
  // Resulting rule type:
  //   if (inner rule == Rule0) Rule0
  //   if (inner rule == Rule1[T]) Rule1[Seq[T]]
  //   if (inner rule == Rule[I, O <: I]) Rule[I, O] // so called "reduction", which leaves the value stack unchanged (on a type level)
  def zeroOrMore[I <: HList, O <: HList](r: Rule[I, O])(implicit s: Sequencer[I, O]): Rule[s.In, s.Out] = notAvailableAtRuntime

  // runs its inner rule until it fails, succeeds if its inner rule succeeded at least once
  // Resulting rule type:
  //   if (inner rule == Rule0) Rule0
  //   if (inner rule == Rule1[T]) Rule1[Seq[T]]
  //   if (inner rule == Rule[I, O <: I]) Rule[I, O] // so called "reduction", which leaves the value stack unchanged (on a type level)
  def oneOrMore[I <: HList, O <: HList](r: Rule[I, O])(implicit s: Sequencer[I, O]): Rule[s.In, s.Out] = notAvailableAtRuntime

  // runs its inner rule but resets the parser (cursor and value stack) afterwards
  // succeeds only if its inner rule succeeded
  def &(r: Rule[_, _]): Rule0 = notAvailableAtRuntime

  // pushes the input text matched by its inner rule onto the value stack
  // after its inner rule has been run successfully
  def capture[I <: HList, O <: HList](r: Rule[I, O])(implicit p: Prepender[O, String :: HNil]): Rule[I, p.Out] = notAvailableAtRuntime

  // pushes the given value onto the value stack
  // if (O == Unit) pushes nothing
  // else if (O <: HList) pushes all values of the HList
  // else pushes a single value of type O
  def push[O](value: O)(implicit j: Join[HNil, HNil, O]): RuleN[j.Out] = notAvailableAtRuntime

  def EOI = org.parboiled2.EOI

  implicit def pimpString(s: String): PimpedString = null
  sealed trait PimpedString {
    def -(other: String): Rule0
  }

  implicit def pimpActionOp[I <: HList, O <: HList](r: Rule[I, O])(implicit ops: ActionOps[I, O]): ActionOpsPimp[I, O, ops.Out] = null
  sealed trait ActionOpsPimp[I <: HList, O <: HList, Ops] {
    def ~> : Ops
  }
}

//////////////////////////////// SUPPORTING TYPE-CLASSES ////////////////////////////////////

sealed trait TailSwitch[L <: HList, T <: HList, R <: HList] {
  // type-level implementation of this logic:
  // Out =
  //   if (T has a tail of type L) R
  //   if (L has a tail of type T) (L dropRight T) ::: R
  type Out <: HList
}
object TailSwitch {
  implicit def tailSwitch[L <: HList, T <: HList, R <: HList, Out0 <: HList](implicit ts: TailSwitch0[L, L, T, T, R, HNil, Out0]) =
    new TailSwitch[L, T, R] {
      type Out = Out0
    }
}

// type-level implementation of this algorithm:
//   @tailrec def rec(L, LI, T, TI, R, RI) =
//     if (TI == L) R
//     else if (LI == T) RI.reverse ::: R
//     else if (LI == HNil) rec(L, HNil, T, TI.tail, R, RI)
//     else if (TI == HNil) rec(L, LI.tail, T, HNil, R, LI.head :: RI)
//     rec(L, LI.tail, T, TI.tail, R, LI.head :: RI)
//   rec(L, L, T, T, R, HNil)
sealed trait TailSwitch0[L <: HList, LI <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList, Out <: HList]
object TailSwitch0 extends TailSwitch0_1 {
  implicit def terminate1[L <: HList, LI <: HList, T <: HList, R <: HList, RI <: HList]: TailSwitch0[L, LI, T, L, R, RI, R] = null
}
private[parboiled2] abstract class TailSwitch0_1 extends TailSwitch0_2 {
  implicit def terminate2[L <: HList, T <: HList, TI <: HList, R <: HList, RI <: HList, Out <: HList](implicit rp: ReversePrependAux[RI, R, Out]): TailSwitch0[L, T, T, TI, R, RI, Out] = null
}
private[parboiled2] abstract class TailSwitch0_2 {
  implicit def iter1[L <: HList, T <: HList, TH, TT <: HList, R <: HList, RI <: HList, Out <: HList](implicit next: TailSwitch0[L, HNil, T, TT, R, RI, Out]): TailSwitch0[L, HNil, T, TH :: TT, R, RI, Out] = null
  implicit def iter2[L <: HList, LH, LT <: HList, T <: HList, R <: HList, RI <: HList, Out <: HList](implicit next: TailSwitch0[L, LT, T, HNil, R, LH :: RI, Out]): TailSwitch0[L, LH :: LT, T, HNil, R, RI, Out] = null
  implicit def iter3[L <: HList, LH, LT <: HList, T <: HList, TH, TT <: HList, R <: HList, RI <: HList, Out <: HList](implicit next: TailSwitch0[L, LT, T, TT, R, LH :: RI, Out]): TailSwitch0[L, LH :: LT, T, TH :: TT, R, RI, Out] = null
}

sealed trait ActionOps[I <: HList, O <: HList] { type Out }
object ActionOps {
  private type ToHList[R] = Join[HNil, HNil, R]

  implicit def ops0[I <: HList, O <: HNil] = new ActionOps[I, O] { type Out = Ops0[I] }
  sealed trait Ops0[I <: HList] {
    def apply[R](f: () ⇒ R)(implicit h: ToHList[R]): Rule[I, h.Out]
    def apply[Z, R](f: Z ⇒ R)(implicit h: ToHList[R]): Rule[Z :: I, h.Out]
    def apply[Y, Z, R](f: (Y, Z) ⇒ R)(implicit h: ToHList[R]): Rule[Y :: Z :: I, h.Out]
    def apply[X, Y, Z, R](f: (X, Y, Z) ⇒ R)(implicit h: ToHList[R]): Rule[X :: Y :: Z :: I, h.Out]
    def apply[W, X, Y, Z, R](f: (W, X, Y, Z) ⇒ R)(implicit h: ToHList[R]): Rule[W :: X :: Y :: Z :: I, h.Out]
    def apply[V, W, X, Y, Z, R](f: (V, W, X, Y, Z) ⇒ R)(implicit h: ToHList[R]): Rule[V :: W :: X :: Y :: Z :: I, h.Out]
  }
  implicit def ops1[I <: HList, A] = new ActionOps[I, A :: HNil] { type Out = Ops1[I, A] }
  sealed trait Ops1[I <: HList, A] {
    def apply[R](f: () ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: h.Out]
    def apply[R](f: A ⇒ R)(implicit h: ToHList[R]): Rule[I, h.Out]
    def apply[Z, R](f: (Z, A) ⇒ R)(implicit h: ToHList[R]): Rule[Z :: I, h.Out]
    def apply[Y, Z, R](f: (Y, Z, A) ⇒ R)(implicit h: ToHList[R]): Rule[Y :: Z :: I, h.Out]
    def apply[X, Y, Z, R](f: (X, Y, Z, A) ⇒ R)(implicit h: ToHList[R]): Rule[X :: Y :: Z :: I, h.Out]
    def apply[W, X, Y, Z, R](f: (W, X, Y, Z, A) ⇒ R)(implicit h: ToHList[R]): Rule[W :: X :: Y :: Z :: I, h.Out]
  }
  implicit def ops2[I <: HList, A, B] = new ActionOps[I, A :: B :: HNil] { type Out = Ops2[I, A, B] }
  sealed trait Ops2[I <: HList, A, B] {
    def apply[R](f: () ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: B :: h.Out]
    def apply[R](f: B ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: h.Out]
    def apply[R](f: (A, B) ⇒ R)(implicit h: ToHList[R]): Rule[I, h.Out]
    def apply[Z, R](f: (Z, A, B) ⇒ R)(implicit h: ToHList[R]): Rule[Z :: I, h.Out]
    def apply[Y, Z, R](f: (Y, Z, A, B) ⇒ R)(implicit h: ToHList[R]): Rule[Y :: Z :: I, h.Out]
    def apply[X, Y, Z, R](f: (X, Y, Z, A, B) ⇒ R)(implicit h: ToHList[R]): Rule[X :: Y :: Z :: I, h.Out]
  }
  implicit def ops3[I <: HList, A, B, C] = new ActionOps[I, A :: B :: C :: HNil] { type Out = Ops3[I, A, B, C] }
  sealed trait Ops3[I <: HList, A, B, C] {
    def apply[R](f: () ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: B :: C :: h.Out]
    def apply[R](f: C ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: B :: h.Out]
    def apply[R](f: (B, C) ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: h.Out]
    def apply[R](f: (A, B, C) ⇒ R)(implicit h: ToHList[R]): Rule[I, h.Out]
    def apply[Z, R](f: (Z, A, B, C) ⇒ R)(implicit h: ToHList[R]): Rule[Z :: I, h.Out]
    def apply[Y, Z, R](f: (Y, Z, A, B, C) ⇒ R)(implicit h: ToHList[R]): Rule[Y :: Z :: I, h.Out]
  }
  implicit def ops4[I <: HList, A, B, C, D] = new ActionOps[I, A :: B :: C :: D :: HNil] { type Out = Ops4[I, A, B, C, D] }
  sealed trait Ops4[I <: HList, A, B, C, D] {
    def apply[R](f: () ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: B :: C :: D :: h.Out]
    def apply[R](f: D ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: B :: C :: h.Out]
    def apply[R](f: (C, D) ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: B :: h.Out]
    def apply[R](f: (B, C, D) ⇒ R)(implicit h: ToHList[R]): Rule[I, A :: h.Out]
    def apply[R](f: (A, B, C, D) ⇒ R)(implicit h: ToHList[R]): Rule[I, h.Out]
    def apply[Z, R](f: (Z, A, B, C, D) ⇒ R)(implicit h: ToHList[R]): Rule[Z :: I, h.Out]
  }
  implicit def ops[I <: HList, O <: HList, OI <: HList, A, B, C, D, E](implicit x: TakeRight5[O, OI, A, B, C, D, E]) = new ActionOps[I, O] { type Out = Ops[I, OI, A, B, C, D, E] }
  sealed trait Ops[I <: HList, OI <: HList, A, B, C, D, E] {
    def apply[R](f: () ⇒ R)(implicit j: Join[OI, A :: B :: C :: D :: E :: HNil, R]): Rule[I, j.Out]
    def apply[R](f: E ⇒ R)(implicit j: Join[OI, A :: B :: C :: D :: HNil, R]): Rule[I, j.Out]
    def apply[R](f: (D, E) ⇒ R)(implicit j: Join[OI, A :: B :: C :: HNil, R]): Rule[I, j.Out]
    def apply[R](f: (C, D, E) ⇒ R)(implicit j: Join[OI, A :: B :: HNil, R]): Rule[I, j.Out]
    def apply[R](f: (B, C, D, E) ⇒ R)(implicit j: Join[OI, A :: HNil, R]): Rule[I, j.Out]
    def apply[R](f: (A, B, C, D, E) ⇒ R)(implicit j: Join[OI, HNil, R]): Rule[I, j.Out]
  }
}

// builds L1 ::: L2 ::: convert(R),
// whereby convert(R) =
//   if (R == Unit) HNil
//   else if (R <: HList) R
//   else R :: HNil
sealed trait Join[L1 <: HList, L2 <: HList, R] {
  type Out <: HList
}
object Join {
  implicit def join[L1 <: HList, L2 <: HList, R, Out0 <: HList](implicit x: Join0[L1, L2, R, HNil, Out0]) = new Join[L1, L2, R] { type Out = Out0 }
}
sealed trait Join0[L1 <: HList, L2 <: HList, R, Acc <: HList, Out <: HList]
object Join0 extends LowerPriorityJoin0 {
  implicit def terminate[Acc <: HList, Out0 <: HList](implicit x: Reverse0[HNil, Acc, Out0]): Join0[HNil, HNil, HNil, Acc, Out0] = null
  implicit def iter1[H, T <: HList, L <: HList, R <: HList, Acc <: HList, Out0 <: HList](implicit x: Join0[T, L, R, H :: Acc, Out0]): Join0[H :: T, L, R, Acc, Out0] = null
  implicit def iter2[H, T <: HList, R <: HList, Acc <: HList, Out0 <: HList](implicit x: Join0[HNil, T, R, H :: Acc, Out0]): Join0[HNil, H :: T, R, Acc, Out0] = null
  implicit def iter3[H, T <: HList, Acc <: HList, Out0 <: HList](implicit x: Join0[HNil, HNil, T, H :: Acc, Out0]): Join0[HNil, HNil, H :: T, Acc, Out0] = null
  implicit def convertUnit[L1 <: HList, L2 <: HList, Acc <: HList, Out0 <: HList](implicit x: Join0[L1, L2, HNil, Acc, Out0]): Join0[L1, L2, Unit, Acc, Out0] = null
}
private[parboiled2] abstract class LowerPriorityJoin0 {
  implicit def convertAny[L1 <: HList, L2 <: HList, R, Acc <: HList, Out0 <: HList](implicit x: Join0[L1, L2, R :: HNil, Acc, Out0]): Join0[L1, L2, R, Acc, Out0] = null
}

sealed trait Optionalizer[I <: HList, O <: HList] {
  type In <: HList
  type Out <: HList
}
object Optionalizer extends LowerPriorityOptionalizer {
  implicit object forRule0 extends Optionalizer[HNil, HNil] {
    type In = HNil
    type Out = HNil
  }
  implicit def forRule1[T] = new Optionalizer[HNil, T :: HNil] {
    type In = HNil
    type Out = Option[T] :: HNil
  }
}
private[parboiled2] abstract class LowerPriorityOptionalizer {
  implicit def forReduction[L <: HList, R <: L] = new Optionalizer[L, R] {
    type In = L
    type Out = R
  }
}

sealed trait Sequencer[I <: HList, O <: HList] {
  type In <: HList
  type Out <: HList
}
object Sequencer extends LowerPrioritySequencer {
  implicit object forRule0 extends Sequencer[HNil, HNil] {
    type In = HNil
    type Out = HNil
  }
  implicit def forRule1[T] = new Sequencer[HNil, T :: HNil] {
    type In = HNil
    type Out = Seq[T] :: HNil
  }
}
private[parboiled2] abstract class LowerPrioritySequencer {
  implicit def forReduction[L <: HList, R <: L] = new Sequencer[L, R] {
    type In = L
    type Out = R
  }
}

sealed trait Prepender[P <: HList, S <: HList] {
  type Out <: HList
  def apply(prefix: P, suffix: S): Out
}
object Prepender {
  implicit def hnilPrepend[P <: HList, S <: HNil] = new Prepender[P, S] {
    type Out = P
    def apply(prefix: P, suffix: S) = prefix
  }
  implicit def apply[P <: HList, S <: HList, Out0 <: HList](implicit prepend: PrependAux[P, S, Out0]) =
    new Prepender[P, S] {
      type Out = Out0
      def apply(prefix: P, suffix: S): Out = prepend(prefix, suffix)
    }
}

sealed trait TakeRight5[L <: HList, Init <: HList, A, B, C, D, E]
object TakeRight5 extends LowerPriorityMatchRight5 {
  implicit def forHList5[A, B, C, D, E]: TakeRight5[A :: B :: C :: D :: E :: HNil, HNil, A, B, C, D, E] = null
}
private[parboiled2] abstract class LowerPriorityMatchRight5 {
  implicit def forHList[H, T <: HList, Init <: HList, A, B, C, D, E](implicit x: TakeRight5[T, Init, A, B, C, D, E]): TakeRight5[H :: T, H :: Init, A, B, C, D, E] = null
}