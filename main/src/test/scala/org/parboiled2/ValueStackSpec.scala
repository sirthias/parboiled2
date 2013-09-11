package org.parboiled2

import shapeless._

class ValueStackSpec extends TestParserSpec {
  "The value stack in new parboiled parser" should {
    "successfully push and pop" in new TestParser[Int :: HNil] {
      def a = rule { capture("42") }
      def b = rule { a ~> (_.toInt) }
      def targetRule = b

      "42" must beMatchedBy(42)
    }

    "successfully pop two elements and push at once (reduction)" in new TestParser[Int :: HNil] {
      def a = rule { capture("42") ~ capture("47") ~> ((x: String, y) ⇒ x.toInt + y.toInt) }
      def targetRule = a

      "4247" must beMatchedBy(42 + 47)
    }

    "successfully pop generic collection" in new TestParser[Int :: HNil] {
      def a = rule { capture("42") ~> (x ⇒ List(x.toInt)) }
      def targetRule = rule { a ~> ((x: Seq[Int]) ⇒ x.head) }

      "42" must beMatchedBy(42)
    }

    "successfully workout `zeroOrMore` reduction" in new TestParser[Int :: HNil] {
      def a = rule { "a" ~ push(1) }
      def targetRule = rule { a ~ zeroOrMore(a ~> ((x: Int, y) ⇒ x + y)) }

      "a" must beMatchedBy(1)
      "aa" must beMatchedBy(2)
      "aaa" must beMatchedBy(3)
    }

    "successfully workout `optional` reduction" in new TestParser[String :: HNil] {
      def targetRule = rule { capture("x") ~ optional(capture("0" - "9") ~> ((_: String) + "+" + _)) }

      "x" must beMatchedBy("x")
      "x1" must beMatchedBy("x+1")
    }

    "handle `pop` function of `Unit` result type" in new TestParser[HNil] {
      var sideEffectedVar = "a"
      def a = rule { capture("x") ~> (sideEffectedVar = _) }

      def targetRule = a

      "x" must beMatchedWith(HNil)
      sideEffectedVar mustEqual "x"
    }

    "handle `capture` of `ANY`" in new TestParser[String :: HNil] {
      def targetRule = rule { capture(ANY) }

      "x" must beMatchedBy("x")
      s"$EOI" must beMismatched
    }

    "work with custom AST nodes" in {
      case class CustomString(s: String)

      "simple producing" in new TestParser[CustomString :: HNil] {
        def a = rule { capture("a") ~> (CustomString(_)) }
        def targetRule = a

        "a" must beMatchedBy(CustomString("a"))
      }

      "`capture(optional(..))` combination" in new TestParser[CustomString :: HNil] {
        def a = rule { capture(optional("a")) ~> (CustomString(_)) }
        def targetRule = a

        "b" must beMatchedBy(CustomString(""))
        "a" must beMatchedBy(CustomString("a"))
      }

      "`optional(capture(...))` combination" in new TestParser[CustomString :: HNil] {
        def a = rule { optional(capture("a")) ~> (x ⇒ CustomString(x getOrElse "?")) }
        def targetRule = a

        "b" must beMatchedBy(CustomString("?"))
        "a" must beMatchedBy(CustomString("a"))
      }

      "push nothing with raw `optional`" in new TestParser[CustomString :: HNil] {
        def a = rule { optional("-") ~ optional(capture("1")) ~> (x ⇒ CustomString(x getOrElse "?")) }
        def targetRule = a

        "1" must beMatchedBy(CustomString("1"))
        "-1" must beMatchedBy(CustomString("1"))
        "a" must beMatchedBy(CustomString("?"))
        "-a" must beMatchedBy(CustomString("?"))
      }
    }

    "work with `push`" in new TestParser[Boolean :: HNil] {
      def targetRule = rule { str("true") ~ push(true) }

      "true" must beMatchedBy(true)
    }
  }
}
