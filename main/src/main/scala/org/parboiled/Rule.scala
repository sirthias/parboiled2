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

package org.parboiled

/**
 * Phantom type for which no instance can be created.
 * Only used for type-checking during compile-time.
 */
class Rule private (val matched: Boolean) extends AnyVal {
  def ~(that: Rule): Rule = new Rule(this.matched && that.matched)
  def ||(that: Rule): Rule = new Rule(this.matched || that.matched)
}

object Rule {
  class NotAvailableAtRuntimeException extends RuntimeException

  def apply(matched: Boolean): Rule = new Rule(matched)

  def apply(): Rule = throw new NotAvailableAtRuntimeException

  val failure = new Rule(false)

  val success = new Rule(true)
}