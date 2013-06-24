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

import scala.reflect.macros.{ TypecheckException, Context }

/**
 * A utility which ensures that a code fragment does not typecheck.
 * See: http://stackoverflow.com/questions/15125457/testing-an-assertion-that-something-must-not-compile
 *
 * Credit: Stefan Zeiger (@StefanZeiger) and Miles Sabin (@milessabin)
 */
object CompilerError {
  def verify(code: String): Unit = macro applyImplNoExp
  def verify(code: String, expectedErrorMsg: String): Unit = macro applyImpl

  def applyImplNoExp(c: Context)(code: c.Expr[String]) = applyImpl(c)(code, null)

  def applyImpl(c: Context)(code: c.Expr[String], expectedErrorMsg: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code
    val expected = expectedErrorMsg match {
      case null                               ⇒ ""
      case Expr(Literal(Constant(s: String))) ⇒ s
    }

    try {
      c.typeCheck(c.parse("{ " + codeStr + " }"))
      c.abort(c.enclosingPosition, "Compiler error expected but not encountered. Expected: " + expected)
    } catch {
      case e: TypecheckException if expected.isEmpty                ⇒ // any exception is fine
      case e: TypecheckException if e.getMessage.contains(expected) ⇒ // ok
      case e: TypecheckException ⇒
        c.abort(c.enclosingPosition,
          s"Encountered compiler error differs from expected:\n" +
            s"  Actual: ${e.getMessage}\n" +
            s"  Expected that error message contains: $expected")
    }
    reify(())
  }
}