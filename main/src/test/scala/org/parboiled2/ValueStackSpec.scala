package org.parboiled2

import shapeless._

class ValueStackSpec extends TestParserSpec {
  "The value stack in new parboiled parser" should {
    "successfully push and pop" in new TestParser[Int :: HNil] {
      def a = rule { capture("42") }
      def b = rule { a ~> (_.toInt) }
      def targetRule = b

      "42" must beMatchedWith(42 :: HNil)
    }

    "successfully pop two elements and push at once (reduction)" in new TestParser[Int :: HNil] {
      def a = rule { capture("42") ~ capture("47") ~> ((x: String, y) => x.toInt + y.toInt) }
      def targetRule = a

      "4247" must beMatchedWith((42 + 47) :: HNil)
    }

    "successfully pop generic collection" in new TestParser[Int :: HNil] {
      def a: Rule1[Seq[Int]] = rule { capture("42") ~> ((x: String) => List(x.toInt)) }
      def targetRule = rule { a ~> ((x: Seq[Int]) => x.head) }

      "42" must beMatchedWith(42 :: HNil)
    }

    "successfully workout `zeroOrMore` reduction" in new TestParser[Int :: HNil] {
      def a = rule { "a" ~ push(1) }
      def targetRule = rule { a ~ zeroOrMore(a ~> ((x: Int, y) => x + y)) }

      "a" must beMatchedWith(1 :: HNil)
      "aa" must beMatchedWith(2 :: HNil)
      "aaa" must beMatchedWith(3 :: HNil)
    }

    "successfully workout `optional` reduction" in new TestParser[String :: HNil] {
      def targetRule = rule { capture("x") ~ optional(capture("0" - "9") ~> ((x: String, y) => x + "+" + y)) }

      "x" must beMatchedWith("x" :: HNil)
      "x1" must beMatchedWith("x+1" :: HNil)
    }

    "handle `pop` function of `Unit` result type" in new TestParser[HNil] {
      var sideEffectedVar = "a"
      def a = rule { capture("x") ~> (sideEffectedVar = _) }

      def targetRule = a

      "x" must beMatchedWith(HNil)
      sideEffectedVar mustEqual "x"
    }

    "work with custom AST nodes" in {
        case class CustomString(s: String)

        "simple producing" in new TestParser[CustomString :: HNil] {
          def a = rule { capture("a") ~> (CustomString(_)) }
          def targetRule = a

          "a" must beMatchedWith(CustomString("a") :: HNil)
        }

        "`capture(optional(..))` combination" in new TestParser[CustomString :: HNil] {
          def a = rule { capture(optional("a")) ~> (CustomString(_)) }
          def targetRule = a

          "b" must beMatchedWith(CustomString("") :: HNil)
          "a" must beMatchedWith(CustomString("a") :: HNil)
        }

        "`optional(capture(...))` combination" in new TestParser[CustomString :: HNil] {
          def targetRule = rule { optional(capture("a")) ~> ((x: Option[String]) => CustomString(x.getOrElse("?"))) }

          "b" must beMatchedWith(CustomString("?") :: HNil)
          "a" must beMatchedWith(CustomString("a") :: HNil)
        }

        "push nothing with raw `optional`" in new TestParser[CustomString :: HNil] {
          def targetRule = rule { optional("-") ~ optional(capture("1")) ~>
                                  ((x: Option[String]) => CustomString(x.getOrElse("?"))) }

          "1" must beMatchedWith(CustomString("1") :: HNil)
          "-1" must beMatchedWith(CustomString("1") :: HNil)
          "a" must beMatchedWith(CustomString("?") :: HNil)
          "-a" must beMatchedWith(CustomString("?") :: HNil)
        }
    }

    "work with `push`" in new TestParser[Boolean :: HNil] {
      def targetRule = rule { str("true") ~ push(true)  }

      "true" must beMatchedWith(true :: HNil)
    }
  }
}
