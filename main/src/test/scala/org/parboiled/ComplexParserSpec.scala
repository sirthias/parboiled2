package org.parboiled

import org.specs2.mutable.Specification

class ComplexParserSpec extends Specification {
  class TestParser(val input: ParserInput) extends Parser {
    def ABC = rule { 'a' ~ 'b' ~ 'c' }
    def complexRule = rule { ABC ~ ABC ~ EOI }
  }

  "The new parboiled parser" should {
    "successfully recognize complex rule" in {
      new TestParser("abcabc").complexRule.isMatched must beTrue
      new TestParser("abcbc").complexRule.isMatched must beFalse
      new TestParser("abcbc").complexRule.isMatched must beFalse
      new TestParser("abc").complexRule.isMatched must beFalse
      new TestParser("y").complexRule.isMatched must beFalse
    }
  }
}