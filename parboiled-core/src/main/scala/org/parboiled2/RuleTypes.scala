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

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{ implicitNotFound, compileTimeOnly }
import scala.collection.immutable
import org.parboiled2.support._
import shapeless._

trait RuleTypes {
  type Context

  type RuleN[+L <: HList] = Rule[Context, HNil, L]
  type Rule1N[-A, +L <: HList] = Rule1X[Context, A, HNil, L]
  type Rule2N[-A, -B, +L <: HList] = Rule2X[Context, A, B, HNil, L]
  type Rule3N[-A, -B, -C, +L <: HList] = Rule3X[Context, A, B, C, HNil, L]

  type Rule0 = RuleN[HNil]
  type Rule1[+T] = RuleN[T :: HNil]
  type Rule2[+A, +B] = RuleN[A :: B :: HNil]
  type Rule3[+A, +B, +C] = RuleN[A :: B :: C :: HNil]

  type Rule10[-A] = Rule1N[A, HNil]
  type Rule11[-A, +T] = Rule1N[A, T :: HNil]
  type Rule12[-A, +T0, +T1] = Rule1N[A, T0 :: T1 :: HNil]
  type Rule13[-A, +T0, +T1, +T2] = Rule1N[A, T0 :: T1 :: T2 :: HNil]

  type Rule20[-A, -B] = Rule2N[A, B, HNil]
  type Rule21[-A, -B, +T] = Rule2N[A, B, T :: HNil]
  type Rule22[-A, -B, +T0, +T1] = Rule2N[A, B, T0 :: T1 :: HNil]
  type Rule23[-A, -B, +T0, +T1, +T2] = Rule2N[A, B, T0 :: T1 :: T2 :: HNil]

  type Rule30[-A, -B, -C] = Rule3N[A, B, C, HNil]
  type Rule31[-A, -B, -C, +T] = Rule3N[A, B, C, T :: HNil]
  type Rule32[-A, -B, -C, +T0, +T1] = Rule3N[A, B, C, T0 :: T1 :: HNil]
  type Rule33[-A, -B, -C, +T0, +T1, +T2] = Rule3N[A, B, C, T0 :: T1 :: T2 :: HNil]
}

sealed trait Rule1X[-Ctx, -A, -I <: HList, +O <: HList] extends (A ⇒ Rule[Ctx, I, O])
sealed trait Rule2X[-Ctx, -A, -B, -I <: HList, +O <: HList] extends ((A, B) ⇒ Rule[Ctx, I, O])
sealed trait Rule3X[-Ctx, -A, -B, -C, -I <: HList, +O <: HList] extends ((A, B, C) ⇒ Rule[Ctx, I, O])

sealed trait Rule[-Ctx, -I <: HList, +O <: HList] {

  @compileTimeOnly("Calls to `~` must be inside `rule` macro")
  def ~[C <: Ctx, I2 <: HList, O2 <: HList](that: Rule[C, I2, O2])(
    implicit i: TailSwitch[I2, O @uncheckedVariance, I @uncheckedVariance],
    o: TailSwitch[O @uncheckedVariance, I2, O2]): Rule[C, i.Out, o.Out] = `n/a`

  @compileTimeOnly("Calls to `~!~` must be inside `rule` macro")
  def ~!~[C <: Ctx, I2 <: HList, O2 <: HList](that: Rule[C, I2, O2])(
    implicit i: TailSwitch[I2, O @uncheckedVariance, I @uncheckedVariance],
    o: TailSwitch[O @uncheckedVariance, I2, O2]): Rule[C, i.Out, o.Out] = `n/a`

  @compileTimeOnly("Calls to `|` must be inside `rule` macro")
  def |[C <: Ctx, I2 <: I, O2 >: O <: HList](that: Rule[C, I2, O2]): Rule[C, I2, O2] = `n/a`

  @compileTimeOnly("Calls to `unary_!` must be inside `rule` macro")
  def unary_!(): Rule[Ctx, HNil, HNil] = `n/a`

  @compileTimeOnly("Calls to `named` must be inside `rule` macro")
  def named(name: String): this.type = `n/a`

  @compileTimeOnly("Calls to `.?` must be inside `rule` macro")
  def ?(implicit l: Lifter[Option, I @uncheckedVariance, O @uncheckedVariance]): Rule[Ctx, l.In, l.OptionalOut] = `n/a`

  @compileTimeOnly("Calls to `.*` must be inside `rule` macro")
  def *(implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]): Rule[Ctx, l.In, l.OptionalOut] with Repeated = `n/a`

  @compileTimeOnly("Calls to `.*` must be inside `rule` macro")
  def *[C <: Ctx](separator: Rule[C, HNil, HNil])(implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]): Rule[C, l.In, l.OptionalOut] = `n/a`

  @compileTimeOnly("Calls to `.+` must be inside `rule` macro")
  def +(implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]): Rule[Ctx, l.In, l.StrictOut] with Repeated = `n/a`

  @compileTimeOnly("Calls to `.+` must be inside `rule` macro")
  def +[C <: Ctx](separator: Rule[C, HNil, HNil])(implicit l: Lifter[immutable.Seq, I @uncheckedVariance, O @uncheckedVariance]): Rule[C, l.In, l.StrictOut] = `n/a`

  /**
   * Runs a parser rule against a given input.
   *
   * @param input the input to run against
   * @param initialValueStackSize the initial size of the value stack for this run
   * @param maxValueStackSize the maximum size of the value stack for this run
   *
   * @param errorTraceCollectionLimit the maximum number of error traces that parser will collect in case of a parse error.
   * Set to zero to completely disable error trace collection (which will cause error formatting to not be able to
   * render any "expected" string!).
   *
   * @return the parsing result according to the implicitly selected DeliveryScheme
   */
  final def run(input: ParserInput,
                errorTraceCollectionLimit: Int = 24,
                initialValueStackSize: Int = 16,
                maxValueStackSize: Int = 1024)(implicit scheme: DeliveryScheme[O @uncheckedVariance],
                                               ev: IsHNil[I @uncheckedVariance],
                                               dv: DefaultValue[Ctx]): scheme.Result =
    runWithContext(input, dv.defaultValue, errorTraceCollectionLimit, initialValueStackSize, maxValueStackSize)

  /**
   * Runs a parser rule against a given input using the given
   *
   * @param input the input to run against
   * @param ctx the custom context for the run
   * @param initialValueStackSize the initial size of the value stack for this run
   * @param maxValueStackSize the maximum size of the value stack for this run
   *
   * @param errorTraceCollectionLimit the maximum number of error traces that parser will collect in case of a parse error.
   * Set to zero to completely disable error trace collection (which will cause error formatting to not be able to
   * render any "expected" string!).
   *
   * @return the parsing result according to the implicitly selected DeliveryScheme
   */
  final def runWithContext(input: ParserInput,
                           ctx: Ctx,
                           errorTraceCollectionLimit: Int = 24,
                           initialValueStackSize: Int = 16,
                           maxValueStackSize: Int = 1024)(implicit scheme: DeliveryScheme[O @uncheckedVariance],
                                                          ev: IsHNil[I @uncheckedVariance]): scheme.Result = {
    require(initialValueStackSize >= 0, "`initialValueStackSize` must be >= 0")
    require(initialValueStackSize >= 0, "`initialValueStackSize` must be >= 0")
    require(maxValueStackSize <= 65536, "`maxValueStackSize` > 2^16 is not supported") // due to current snapshot design
    val state = new ParserStateImpl(input, ctx)
    state.run(this.asInstanceOf[RuleImpl[Ctx]], errorTraceCollectionLimit, initialValueStackSize, maxValueStackSize)
  }
}

abstract class RuleImpl[Ctx] extends Rule[Ctx, Nothing, HNil] {
  def run(psi: ParserStateImpl[Ctx]): Boolean
}

abstract class Rule1XImpl[Ctx, -A] extends Rule1X[Ctx, A, Nothing, HNil] {
  def run(a: A, psi: ParserStateImpl[Ctx]): Boolean
  def apply(a: A): Rule[Ctx, Nothing, HNil] = `n/a`
}

abstract class Rule2XImpl[Ctx, -A, -B] extends Rule2X[Ctx, A, B, Nothing, HNil] {
  def run(a: A, b: B, psi: ParserStateImpl[Ctx]): Boolean
  def apply(a: A, b: B): Rule[Ctx, Nothing, HNil] = `n/a`
}

abstract class Rule3XImpl[Ctx, -A, -B, -C] extends Rule3X[Ctx, A, B, C, Nothing, HNil] {
  def run(a: A, b: B, c: C, psi: ParserStateImpl[Ctx]): Boolean
  def apply(a: A, b: B, c: C): Rule[Ctx, Nothing, HNil] = `n/a`
}

// phantom type for WithSeparatedBy pimp
trait Repeated

@implicitNotFound("Rules requiring input from the value stack cannot be run as top-level rules!")
sealed trait IsHNil[I <: HList] // phantom type
object IsHNil {
  implicit val forHNil: IsHNil[HNil] = null
}

@implicitNotFound("You cannot use `rule.run` with a custom Context. Use `rule.runWithContext` instead!")
class DefaultValue[+T](val defaultValue: T)
object DefaultValue {
  implicit val defaultAny = new DefaultValue[Any](())
}
