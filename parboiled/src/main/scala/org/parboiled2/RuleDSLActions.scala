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

import scala.reflect.internal.annotations.compileTimeOnly
import shapeless.ops.hlist.Prepend
import org.parboiled2.support._
import shapeless._

trait RuleDSLActions {

  /**
   * Pushes the input text matched by its inner rule onto the value stack
   * after its inner rule has been run successfully (and only then).
   */
  @compileTimeOnly("Calls to `capture` must be inside `rule` macro")
  def capture[I <: HList, O <: HList](r: Rule[I, O])(implicit p: Prepend[O, String :: HNil]): Rule[I, p.Out] = `n/a`

  /**
   * Implements a semantic predicate. If the argument expression evaluates to `true` the created
   * rule matches otherwise it doesn't.
   */
  @compileTimeOnly("Calls to `test` must be inside `rule` macro")
  def test(condition: Boolean): Rule0 = `n/a`

  /**
   * Simply runs the given block / expression. The valueStack is left untouched.
   * NOTE: Even though the block is not a call-by-name parameter it will be executed
   * for every rule application anew! (Since the expression is directly transplanted
   * into the rule method by the `rule` macro.
   */
  @compileTimeOnly("Calls to `run` must be inside `rule` macro")
  def run(block: Unit): Rule0 = `n/a`

  /**
   * Pushes the given value onto the value stack.
   * - if `T` is `Unit` nothing is pushed (i.e. `push` with a block/expression evaluating to `Unit` is identical to `run`)
   * - if `T <: HList` all values of the HList is pushed as individual elements
   * - otherwise a single value of type `T` is pushed.
   */
  @compileTimeOnly("Calls to `push` must be inside `rule` macro")
  def push[T](value: T)(implicit h: HListable[T]): RuleN[h.Out] = `n/a`

  @compileTimeOnly("Calls to `rule2ActionOperator` must be inside `rule` macro")
  implicit def rule2ActionOperator[I <: HList, O <: HList](r: Rule[I, O])(implicit ops: ActionOps[I, O]): ActionOperator[I, O, ops.Out] = `n/a`
  sealed trait ActionOperator[I <: HList, O <: HList, Ops] {
    def ~> : Ops
  }
}