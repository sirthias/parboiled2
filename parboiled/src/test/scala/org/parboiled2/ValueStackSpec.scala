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

    "handle `pop` function of multiple expressions with a non-`Rule` result" in new TestParser[Int :: HNil] {
      var sideEffectedVar = "a"

      def targetRule = rule { capture("7") ~> { sideEffectedVar = "b"; (_: String).toInt } }

      "x" must beMismatched
      sideEffectedVar mustEqual "a"
      "7" must beMatchedBy(7)
      sideEffectedVar mustEqual "b"
    }

    "handle `pop` function of expression block with a `Rule1` result" in new TestParser[Int :: Int :: HNil] {
      var sideEffectedVar = "a"

      def targetRule = rule { capture("7") ~> ((x: String) ⇒ { sideEffectedVar = "b"; push(42 :: x.toInt :: HNil) }) }

      "x" must beMismatched
      sideEffectedVar mustEqual "a"
      "7" must beMatchedWith(42 :: 7 :: HNil)
      sideEffectedVar mustEqual "b"
    }

    "handle `pop` function of expression block with a `Unit` result" in new TestParser[HNil] {
      var sideEffectedVar = "a"

      def targetRule = rule { capture("7") ~> ((x: String) ⇒ { sideEffectedVar = "b"; sideEffectedVar = "c" }) }

      "x" must beMismatched
      sideEffectedVar mustEqual "a"
      "7" must beMatchedWith(HNil)
      sideEffectedVar mustEqual "c"
    }

    "handle `pop` function with non-Rule result from block of expressions" in new TestParser[Int :: HNil] {
      var sideEffectedVar = "a"

      def targetRule = rule {
        capture("7") ~> ((x: String) ⇒ {
          sideEffectedVar = "9"

          {
            val r1 = x.toInt
            val r2 = sideEffectedVar.toInt
            r1 + r2
          }
        })
      }

      "x" must beMismatched
      sideEffectedVar mustEqual "a"
      "7" must beMatchedBy(7 + 9)
      sideEffectedVar mustEqual "9"
    }

    "handle `capture` of `ANY`" in new TestParser[String :: HNil] {
      def targetRule = rule { capture(ANY) }

      "x" must beMatchedBy("x")
      "" must beMismatched
    }

    "handle `nTimes` of `times == 0` with rule of type `Rule1`" in new TestParser[Seq[Int]:: HNil] {
      def targetRule = rule { nTimes(0, capture("7") ~> (_.toInt)) ~ EOI }

      "6" must beMismatched
      "" must beMatchedBy(Seq())
    }

    "handle `nTimes` with rule of type `Rule1`" in new TestParser[Seq[Int]:: HNil] {
      def targetRule = rule { nTimes(2, capture("7") ~> (_.toInt)) }

      "76" must beMismatched
      "77" must beMatchedBy(Seq(7, 7))
    }

    "handle `capture` of `nTimes`" in new TestParser[String :: HNil] {
      def targetRule = rule { capture(nTimes(2, "a")) ~ EOI }

      "a" must beMismatched
      "aa" must beMatchedBy("aa")
    }

    "handle `nTimes` of `capture`" in new TestParser[Seq[String]:: HNil] {
      def targetRule = rule { nTimes(2, capture("a")) ~ EOI }

      "a" must beMismatched
      "aa" must beMatchedBy(Seq("a", "a"))
    }

    "work with custom AST nodes" in {
      case class ParameterLess()
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

      "parameterless case class application" in new TestParser[ParameterLess :: HNil] {
        def targetRule = rule { str("a") ~> ParameterLess }

        "a" must beMatchedBy(ParameterLess())
      }

      "parameterful case class application" in new TestParser[CustomString :: HNil] {
        def targetRule = rule { capture("a") ~> CustomString }

        "a" must beMatchedBy(CustomString("a"))
      }

      "partially consumes values from value stack" in new TestParser[String :: CustomString :: HNil] {
        def targetRule = rule { capture("a") ~ capture("b") ~> CustomString }

        "ab" must beMatchedWith("a" :: CustomString("b") :: HNil)
      }
    }

    "have growing order from left to right" in new TestParser[String :: Int :: HNil] {
      def targetRule = rule { capture("a") ~ push(1) }

      "a" must beMatchedWith("a" :: 1 :: HNil)
    }

    "work with `push`" in {
      "of arity 1" in new TestParser[Boolean :: HNil] {
        def targetRule = rule { str("true") ~ push(true) }

        "true" must beMatchedBy(true)
      }

      "of arity 2+" in new TestParser[Boolean :: Int :: Double :: HNil] {
        def targetRule = rule { str("true") ~ push(true :: 7 :: 42.0 :: HNil) }

        "true" must beMatchedWith(true :: 7 :: 42.0 :: HNil)
      }
    }

    "pass integration tests" in {
      "sequence rule in `~>` production" in new TestParser[HNil] {
        def foo = rule { capture("a") ~ capture("b") ~> (str(_) ~ _) }
        def targetRule = foo

        "abab" must beMatchedWith(HNil)
        "ab" must beMismatched
      }

      "complex rule" in new TestParser[String :: Int :: Int :: HNil] {
        def bar = rule { push(5) ~ capture("ab") ~ foo }
        def foo = rule { push(4) ~> ((s1: String, i) ⇒ capture(s1) ~> ((j: Int, s2) ⇒ push(s2 :: j :: i :: HNil))) }
        def targetRule = bar

        "abab" must beMatchedWith("ab" :: 5 :: 4 :: HNil)
      }
    }
  }
}
