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

import org.parboiled2.support.hlist._
import utest._

object ActionSpec extends TestParserSpec {

  val tests = Tests {

    "The Parser should correctly handle" - {
      "`capture` simple" - new TestParser1[String] {
        def targetRule = rule(capture('b'))
        "b" must beMatchedWith("b")
      }

      "`capture`" - new TestParser1[String] {
        def targetRule = rule('a' ~ capture(zeroOrMore('b')) ~ EOI)
        "a" must beMatchedWith("")
        "b" must beMismatched
        "ab" must beMatchedWith("b")
        "abb" must beMatchedWith("bb")
      }

      "`test`" - new TestParser0 {
        var flag       = true
        def targetRule = rule(test(flag))
        "x" must beMatched
        flag = false
        "x" must beMismatched
      }

      "`run(nonRuleExpr)`" - new TestParser0 {
        var flag       = false
        def targetRule = rule('a' ~ run { flag = true } ~ EOI)
        "a" must beMatched
        assert(flag)
      }

      "`run(ruleBlockWithRuleCall)`" - new TestParser0 {
        var flag       = false
        def targetRule = rule('a' ~ run { flag = true; b } ~ EOI)
        def b          = rule('b')
        "a" must beMismatched
        assert(flag)
        "ab" must beMatched
      }

      "`run(ruleBlockWithNestedRuleDef)`" - new TestParser0 {
        var flag       = false
        def targetRule = rule('a' ~ run { flag = true; ch('b') } ~ EOI)
        "a" must beMismatched
        assert(flag)
        "ab" must beMatched
      }

      "`run(ruleBlockWithRuleIf)`" - new TestParser0 {
        var flag       = false
        def targetRule = rule('a' ~ run { flag = true; if (flag) oneOrMore(b) else MISMATCH } ~ EOI)
        def b          = rule('b')
        "a" must beMismatched
        assert(flag)
        "abbb" must beMatched
      }

      "`run(ruleBlockWithRuleMatch)`" - new TestParser0 {
        var flag = false
        def targetRule =
          rule {
            'a' ~ run { flag = true; flag match { case true => oneOrMore(b); case _ => MISMATCH } } ~ EOI
          }
        def b = rule('b')
        "a" must beMismatched
        assert(flag)
        "abbb" must beMatched
      }

      "`run(F1producingUnit)`" - new TestParser1[Int] {
        def targetRule = rule(push(1 :: "X" :: HNil) ~ run((x: String) => require(x == "X")) ~ EOI)
        "" must beMatchedWith(1)
      }

      "`run(F2producingValue)`" - new TestParser1[Char] {
        def targetRule = rule(push(1 :: "X" :: HNil) ~ run((i: Int, x: String) => (x.head - i).toChar) ~ EOI)
        "" must beMatchedWith('W')
      }

      "`run(F2producingHList)`" - new TestParserN[String :: Int :: HNil] {
        def targetRule = rule(push(1 :: "X" :: HNil) ~ run((i: Int, x: String) => x :: i :: HNil) ~ EOI)
        "" must beMatchedWith("X" :: 1 :: HNil)
      }

      // FIXME: problem with TailSwitch, type error
      /*"`run(F1producingRule)`" - new TestParser0 {
        def targetRule = rule(ANY ~ push(lastChar - '0') ~ run((i: Int) => test(i % 2 == 0)) ~ EOI)
        "4" must beMatched
        "5" must beMismatched
      }*/

      //    "`run(F1TakingHList)`" in new TestParser1[Int] {
      //      def targetRule = rule { push(42 :: "X" :: HNil) ~ run((l: Int :: String :: HNil) => l.head * 2) }
      //      "" must beMatchedWith(84)
      //    }

      "`push` simple value" - new TestParser1[String] {
        def targetRule: Rule1[String] = rule('x' ~ push(()) ~ push(HNil) ~ 'y' ~ push("yeah") ~ EOI)
        "xy" must beMatchedWith("yeah")
      }

      "`push` HList" - new TestParserN[Int :: Double :: Long :: String :: HNil] {
        def targetRule = rule('x' ~ push(42 :: 3.14 :: HNil) ~ push(0L :: "yeah" :: HNil) ~ EOI)
        "x" must beMatchedWith(42 :: 3.14 :: 0L :: "yeah" :: HNil)
        "y" must beMismatched
      }

      "`drop[Int]`" - new TestParser0 {
        def targetRule = rule(push(42) ~ drop[Int] ~ EOI)
        "" must beMatched
      }

      "`drop[Int :: String :: HNil]`" - new TestParser0 {
        def targetRule = rule(push(42 :: "X" :: HNil) ~ drop[Int :: String :: HNil] ~ EOI)
        "" must beMatched
      }

      "`~>` producing `Unit`" - new TestParser1[Int] {
        def testRule   = rule(push(1 :: "X" :: HNil) ~> (_ => ()))
        def targetRule = testRule
        "" must beMatchedWith(1)
      }

      case class Foo(i: Int, s: String)

      /*FIXME: type error "`~>` producing case class (simple notation)" - new TestParser1[Foo] {
        def targetRule = rule(push(1 :: "X" :: HNil) ~> Foo)
        "" must beMatchedWith(Foo(1, "X"))
      }*/

      "`~>` full take" - new TestParser1[Foo] {
        def testRule   = rule(push(1 :: "X" :: HNil) ~> (Foo(_, _)))
        def targetRule = testRule
        "" must beMatchedWith(Foo(1, "X"))
      }

      "`~>` partial take" - new TestParser1[Foo] {
        def testRule   = rule(push(1) ~> (Foo(_, "X")))
        def targetRule = testRule
        "" must beMatchedWith(Foo(1, "X"))
      }

      "`~>` producing HList" - new TestParserN[String :: Int :: Double :: HNil] {
        def testRule   = rule(capture("x") ~> (_ :: 1 :: 3.0 :: HNil))
        def targetRule = testRule
        "x" must beMatchedWith("x" :: 1 :: 3.0 :: HNil)
      }

      /*FIXME: one or more of these don't terminate while typing "`~>` with a statement block" - new TestParser1[Char] {
        var captured = ' '
        def testRule =
          rule {
            capture("x") ~> { x => captured = x.head; cursorChar }
          }
        def targetRule = testRule
        "xy" must beMatchedWith('y')
        captured ==> 'x'
      }

      "`~>` producing a Rule0" - new TestParser0 {
        def testRule   = rule(capture("x") ~> (str(_)) ~ EOI)
        def targetRule = testRule
        "x" must beMismatched
        "xx" must beMatched
      }

      "`~>` producing a Rule1" - new TestParser1[String] {
        def testRule   = rule(capture("x") ~> (capture(_)) ~ EOI)
        def targetRule = testRule
        "x" must beMismatched
        "xx" must beMatchedWith("x")
      }

      "`~>` producing an expression evaluating to a rule" - new TestParser0 {
        def testRule   = rule(capture(anyOf("ab")) ~> (s => if (s == "a") ch('b') else ch('a')) ~ EOI)
        def targetRule = testRule
        "ab" must beMatched
        "ba" must beMatched
        "a" must beMismatched
        "b" must beMismatched
      }*/
    }
  }
}
