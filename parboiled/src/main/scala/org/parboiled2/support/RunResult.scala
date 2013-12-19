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

import org.parboiled2.{ Rule0, RuleX }

// phantom type, only used for rule DSL typing
sealed trait RunResult[T] {
  type Out <: RuleX
}

object RunResult extends LowerPriorityRunResult {
  implicit def forRule[R <: RuleX]: RunResult[R] { type Out = R } = `n/a`
}

abstract class LowerPriorityRunResult {
  implicit def forAny[T]: RunResult[T] { type Out = Rule0 } = `n/a`
}
