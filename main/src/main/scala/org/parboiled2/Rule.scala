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

// TODO: model as value class wrapping a `matched: Boolean`
// This would give us a (small) performance benefit since we'd save the comparison to the predefined
// `Matched` and `Mismatched` objects.
// However, https://issues.scala-lang.org/browse/SI-6260 is quite a serious problem for this design,
// so until this issue is fixed we better stick to this non-value-class-based model
sealed abstract class Rule[L <: HList] {
  import Rule.notAvailableAtRuntime

  def ~[R <: HList](that: Rule[R])(implicit p: Prepender[L, R]): Rule[p.Out] = notAvailableAtRuntime
  def |[R >: L <: HList](that: Rule[R]): Rule[R] = notAvailableAtRuntime
  def unary_!(): Rule0 = notAvailableAtRuntime

  def matched: Boolean = this eq Rule.Matched
  def mismatched: Boolean = this eq Rule.Mismatched
}

private[parboiled2] object Rule {
  class NotAvailableAtRuntimeException private[Rule] () extends RuntimeException

  def notAvailableAtRuntime: Nothing = throw new NotAvailableAtRuntimeException

  private object Matched extends Rule[Nothing]
  private object Mismatched extends Rule[Nothing]

  def apply[L <: HList](): Rule[L] = notAvailableAtRuntime

  def apply[L <: HList](m: Boolean): Rule[L] = if (m) matched else mismatched

  def matched[L <: HList]: Rule[L] = Matched.asInstanceOf[Rule[L]]
  def mismatched[L <: HList]: Rule[L] = Mismatched.asInstanceOf[Rule[L]]
}