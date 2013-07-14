package org.parboiled2

class ErrorReportingSpec extends SimpleCalculatorSpec {
  "Error reporting" should {
    "compose error messages for simple expression" in new TestParser {
      def testRule = rule { oneOrMore("x") ~ ch('a') ~ 'b' ~ 'e' }

      "xxxace" must Mismatch
      val errs = errors()
      errs must not be empty
      errs(0) mustEqual ParserError(Mark(0, 4, 4), "LiteralChar(b)")
      errs(1) mustEqual ParserError(Mark(0, 0, 0), "Sequence(Sequence(Sequence(LiteralString(x),ZeroOrMore(LiteralString(x))),LiteralChar(a)), LiteralChar(b))")
      errs(2) mustEqual ParserError(Mark(0, 0, 0), "Sequence(Sequence(Sequence(Sequence(LiteralString(x),ZeroOrMore(LiteralString(x))),LiteralChar(a)),LiteralChar(b)), LiteralChar(e))")
    }

    "compose error messages for simple calculator" in new SimpleCalculator {
      def testRule: Rule = InputLine

      //"1+2*(3+5))-5" must Mismatch
      "3+*5" must Mismatch
      val errs = errors()
      errs must not be empty
      errs(0) mustEqual ParserError(Mark(0, 1, 1), s"LiteralChar(${this.EOI})")
      errs(1) mustEqual ParserError(Mark(0, 0, 0), s"Sequence(RuleCall(SimpleCalculator.this.Expression), LiteralChar(${this.EOI}))")
    }

    "track lines numbers" in {
      "zero line" in new TestParser {
        def testRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }

        "x\nb\nc" must Mismatch
        val errs = errors()
        errs(0).mark must_== Mark(0, 0, 0)
      }

      "first line" in new TestParser {
        def testRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }

        "a\nx\nc" must Mismatch
        val errs = errors()
        errs(0).mark must_== Mark(1, 1, 2)
      }

      "second line" in new TestParser {
        def testRule: Rule = rule { str("a\n") ~ "b\n" ~ "c" }

        "a\nb\nx" must Mismatch
        val errs = errors()
        errs(0).mark must_== Mark(2, 1, 4)
      }
    }

    "correctly process FirstOf" in {
      "producing no errors for first alternative" in new TestParser {
        def testRule: Rule = rule { ch('a') | 'b' }

        "a" must Match
        errors must be empty
      }

      "producing no errors for second alternative" in new TestParser {
        def testRule: Rule = rule { ch('a') | 'b' }

        "b" must Match
        errors must be empty
      }
    }
  }
}