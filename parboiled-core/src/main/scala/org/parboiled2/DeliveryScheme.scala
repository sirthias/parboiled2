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

import scala.util.{ Failure, Success, Try }
import shapeless._
import org.parboiled2.support.Unpack

trait DeliveryScheme[L <: HList] {
  type Result
  def success(result: L): Result
  def parseError(error: ParseError): Result
  def failure(error: Throwable): Result
}

object DeliveryScheme extends AlternativeDeliverySchemes {
  implicit def Try[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) =
    new DeliveryScheme[L] {
      type Result = Try[Out]
      def success(result: L) = Success(unpack(result))
      def parseError(error: ParseError) = Failure(error)
      def failure(error: Throwable) = Failure(error)
    }
}
sealed abstract class AlternativeDeliverySchemes {
  implicit def Either[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) =
    new DeliveryScheme[L] {
      type Result = Either[ParseError, Out]
      def success(result: L) = Right(unpack(result))
      def parseError(error: ParseError) = Left(error)
      def failure(error: Throwable) = throw error
    }
  implicit def Throw[L <: HList, Out](implicit unpack: Unpack.Aux[L, Out]) =
    new DeliveryScheme[L] {
      type Result = Out
      def success(result: L) = unpack(result)
      def parseError(error: ParseError) = throw error
      def failure(error: Throwable) = throw error
    }
}