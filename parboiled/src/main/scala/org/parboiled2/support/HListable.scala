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

trait HListable[T] {
  type Out <: HList
}

object HListable extends LowerPriorityHListable {
  implicit def fromUnit = new HListable[Unit] { type Out = HNil }
  implicit def fromHList[T <: HList] = new HListable[T] { type Out = T }
}

abstract class LowerPriorityHListable {
  implicit def fromAnyRef[T] = new HListable[T] { type Out = T :: HNil }
}
