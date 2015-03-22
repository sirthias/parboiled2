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

import shapeless.HList

/**
 * Defines the complete environment required for rule definition.
 * Typically you define your parser in an object derived from [[Parser]] like this:
 *
 * {{{
 * object MyParser extends Parser {
 *   // the type of user context required by the parser rules.
 *   // use `Any` if nothing specific is required
 *   type Context = ...
 *
 *   val foo = rule { oneOrMore("foo") }
 * }
 * }}}
 *
 * Has one abstract type member `Context` which defines an upper type bound or concrete type
 * for the user-defined parsing context object that the parser rules defined in this class
 * have access to.
 */
abstract class Parser extends RuleTypes
    with RuleDSLBasics
    with RuleDSLCombinators
    with RuleDSLActions {

  def rule = ruleCreator0
  def rule[A]() = new RuleCreator1[A]("")
  def rule[A, B]() = new RuleCreator2[A, B]("")
  def rule[A, B, C]() = new RuleCreator3[A, B, C]("")

  def namedRule(name: String) = new RuleCreator0(name)
  def namedRule1X[A](name: String) = new RuleCreator1[A](name)
  def namedRule2X[A, B](name: String) = new RuleCreator2[A, B](name)
  def namedRule3X[A, B, C](name: String) = new RuleCreator3[A, B, C](name)

  private val ruleCreator0 = new RuleCreator0("")

  class RuleCreator0(val name: String) {
    def apply[Ctx <: Context, I <: HList, O <: HList](
      r: Rule[Ctx, I, O]): Rule[Ctx, I, O] = macro Macros.rule0[Ctx, I, O]
    def debug[Ctx <: Context, I <: HList, O <: HList](
      r: Rule[Ctx, I, O]): Rule[Ctx, I, O] = macro Macros.debugRule0[Ctx, I, O]
  }
  class RuleCreator1[A](val name: String) {
    def apply[Ctx <: Context, I <: HList, O <: HList](
      r: A ⇒ Rule[Ctx, I, O]): Rule1X[Ctx, A, I, O] = macro Macros.rule1[Ctx, A, I, O]
  }
  class RuleCreator2[A, B](val name: String) {
    def apply[Ctx <: Context, I <: HList, O <: HList](
      r: (A, B) ⇒ Rule[Ctx, I, O]): Rule2X[Ctx, A, B, I, O] = macro Macros.rule2[Ctx, A, B, I, O]
  }
  class RuleCreator3[A, B, C](val name: String) {
    def apply[Ctx <: Context, I <: HList, O <: HList](
      r: (A, B, C) ⇒ Rule[Ctx, I, O]): Rule3X[Ctx, A, B, C, I, O] = macro Macros.rule3[Ctx, A, B, C, I, O]
  }
}

/**
 * Convenience type for simple parsers that don't have any specific requirements on
 * the user context type.
 */
abstract class SimpleParser extends Parser {
  type Context = Any
}

/**
 * Convenience module for simple parsers that don't have any specific requirements on
 * the user context type.
 * Simply `import org.parboiled2.SimpleParser._` and define your rules.
 */
object SimpleParser extends SimpleParser