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
  inline def rule[I <: HList, O <: HList](inline r: Rule[I, O]): Rule[I, O] = ${ ParserMacros.ruleImpl('r) }

  /** Converts a compile-time only rule definition into the corresponding rule method implementation
    * with an explicitly given name.
    */
  inline def namedRule[I <: HList, O <: HList](name: String)(inline r: Rule[I, O]): Rule[I, O] = ${
    ParserMacros.nameRuleImpl('name)('r)
  }

}

private[parboiled2] trait RuleRunnable {

  /** THIS IS NOT PUBLIC API and might become hidden in future. Use only if you know what you are doing!
    */
  implicit class Runnable[L <: HList](rule: RuleN[L]) {
    def run()(implicit scheme: Parser.DeliveryScheme[L]): scheme.Result = ???
  }
}

object ParserMacros {
  import scala.quoted._
  import scala.compiletime._

  def ruleImpl[I <: HList: Type, O <: HList: Type](r: Expr[Rule[I, O]])(using Quotes): Expr[Rule[I, O]] =
    nameRuleImpl(Expr("todo"))(r)

  def nameRuleImpl[I <: HList: Type, O <: HList: Type](
      name: Expr[String]
  )(r: Expr[Rule[I, O]])(using Quotes): Expr[Rule[I, O]] = {
    import quotes.reflect.*
    val opTree: OpTree = r match {
      case '{ ($p: Parser).ch($c) } =>
        println("Char rule!!")
        CharMatch(p, c)
      case _ => reportError("does not understand", name)
    }

    '{
      def wrapped: Boolean = ${ opTree.render(wrapped = true) }
      val matched          = wrapped
      if (matched) org.parboiled2.Rule.asInstanceOf[Rule[I, O]] else null
    }
  }

  private def reportError(error: String, expr: Expr[Any])(using quotes: Quotes): Nothing = {
    quotes.reflect.report.error(error, expr)
    throw new Exception(error)
  }

  sealed trait OpTree {
    def render(wrapped: Boolean)(using Quotes): Expr[Boolean]
  }

  case class CharMatch(parser: Expr[Parser], charTree: Expr[Char]) extends OpTree {
    override def render(wrapped: Boolean)(using Quotes): Expr[Boolean] =
      '{
        $parser.cursorChar == $charTree && $parser.__advance()
      }
  }
}
