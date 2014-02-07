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
import scala.reflect.macros.Context

object DynamicRuleDispatch {
  /**
   * Implements efficient runtime dispatch to a predefined set of parser rules.
   * Given a number of rule names this macro-supported method creates a function instance,
   * which can be used to run one of the respective parser rules given a parser instance and a rule name.
   * Note that there is no reflection involved and compilation will fail, if one of the given rule names
   * does not constitute a method of parser type `T`.
   */
  def apply[P <: Parser, L <: HList](ruleNames: String*): (P, String) ⇒ Option[RunnableRule[P, L]] = macro __create[P, L]

  ///////////////////// INTERNAL ////////////////////////

  def __create[P <: Parser, L <: HList](c: Context)(ruleNames: c.Expr[String]*)(implicit P: c.WeakTypeTag[P], L: c.WeakTypeTag[L]): c.Expr[(P, String) ⇒ Option[RunnableRule[P, L]]] = {
    import c.universe._
    val names: Array[String] = ruleNames.map {
      _.tree match {
        case Literal(Constant(s: String)) ⇒ s
        case x                            ⇒ c.abort(x.pos, s"Invalid `String` argument `x`, only `String` literals are supported!")
      }
    }(collection.breakOut)
    java.util.Arrays.sort(names.asInstanceOf[Array[Object]])

    def rec(start: Int, end: Int): Tree =
      if (start <= end) {
        val mid = (start + end) >>> 1
        val name = names(mid)
        q"""math.signum(${c.literal(name)}.compare(s)) match {
            case -1 => ${rec(mid + 1, end)}
            case 1 => ${rec(start, mid - 1)}
            case 0 => Some {
              new RunnableRule[$P, $L] with Function0[RuleN[$L]] {
                def parserInstance: $P = p
                def apply() = p.${newTermName(name).encodedName}
                override def run()(implicit scheme: Parser.DeliveryScheme[$L]): scheme.Result = p.__run[$L](this.apply)(scheme)
              }
            }
          }"""
      } else q"None"

    c.Expr[(P, String) ⇒ Option[RunnableRule[P, L]]](q"(p: $P, s: String) => ${rec(0, names.length - 1)}")
  }
}

trait RunnableRule[P <: Parser, L <: HList] {
  def parserInstance: P
  def run()(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result
}
