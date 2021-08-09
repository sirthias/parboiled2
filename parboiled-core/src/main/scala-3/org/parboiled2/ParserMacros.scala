/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2

import org.parboiled2.support.hlist.HList

private[parboiled2] trait ParserMacroMethods {

  /** Converts a compile-time only rule definition into the corresponding rule method implementation.
    */
  def rule[I <: HList, O <: HList](r: Rule[I, O]): Rule[I, O] = ???

  /** Converts a compile-time only rule definition into the corresponding rule method implementation
    * with an explicitly given name.
    */
  def namedRule[I <: HList, O <: HList](name: String)(r: Rule[I, O]): Rule[I, O] = ???

}

private[parboiled2] trait RuleRunnable {

  /** THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
    */
  implicit class Runnable[L <: HList](rule: RuleN[L]) {
    def run()(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result = ???
  }
}

object ParserMacros {}
