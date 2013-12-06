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

class ContinuationParserSpec extends TestParserSpec {
  import Parser.Continuation
  import shapeless._

  abstract class ContinuationPassingParser[L <: HList] extends TestParser[L] {
    var sideEffectedVar = "a"

    def SimpleString = rule { "ab" }
    def Seq = rule { str("a") ~ "b" }
    def FirstOf = rule { "a" | "b" }
    def Capture = rule { capture("ab") }
    def Optional = rule { optional("ab") }
    def Action = rule { capture("42") ~> ((_: String).toInt + 11) }
    def ActionUnit = rule { capture("12") ~> (sideEffectedVar = _) }
  }

  type ContinuationPassingParser0 = ContinuationPassingParser[HNil]
  type ContinuationPassingParser1[L] = ContinuationPassingParser[L :: HNil]

  "A ContinuationPassingParser" should {
    "recognize simple string" in new ContinuationPassingParser0 {
      def targetRule = SimpleString

      val Continuation(cont) = startParsing("a", false)
      cont("b", true) must beValueOf(HNil)
    }

    "recognize `sequence`" in new ContinuationPassingParser0 {
      def targetRule = Seq

      val Continuation(cont) = startParsing("a", false)
      cont("b", true) must beValueOf(HNil)
    }

    "recognize `firstOf`" in new ContinuationPassingParser0 {
      def targetRule = FirstOf

      startParsing("a", true) must beValueOf(HNil)
      startParsing("b", true) must beValueOf(HNil)
    }

    "recognize `capture`" in new ContinuationPassingParser1[String] {
      def targetRule = Capture

      val Continuation(cont) = startParsing("a", false)
      cont("b", true) must beValueOf("ab" :: HNil)
    }

    "recognize `optional`" in new ContinuationPassingParser0 {
      def targetRule = Optional

      startParsing("", true) must beValueOf(HNil)

      val Continuation(cont) = startParsing("a", false)
      cont("b", true) must beValueOf(HNil)
    }

    "recognize `action`" in new ContinuationPassingParser1[Int] {
      def targetRule = Action

      val Continuation(cont) = startParsing("4", false)
      cont("2", true) must beValueOf((42 + 11) :: HNil)
    }

    "recognize `action-unit`" in new ContinuationPassingParser0 {
      def targetRule = ActionUnit

      val Continuation(cont) = startParsing("1", false)
      cont("2", true) must beValueOf(HNil)
      sideEffectedVar mustEqual "12"
    }
  }
}