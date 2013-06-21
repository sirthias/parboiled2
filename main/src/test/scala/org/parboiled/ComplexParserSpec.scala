package org.parboiled

import org.specs2.mutable.Specification

class ComplexParserSpec extends Specification {
  class TestParser(val input: ParserInput) extends Parser {
    def ABC = rule { 'a' ~ 'b' ~ 'c' }
    def complexRule = rule { ABC ~ ABC ~ EOI }
  }

  "The new parboiled parser" should {
    "successfully recognize complex rule" in {
      new TestParser("abcabc").complexRule.matched must beTrue
      new TestParser("abcbc").complexRule.matched must beFalse
      new TestParser("abcbc").complexRule.matched must beFalse
      new TestParser("abc").complexRule.matched must beFalse
      new TestParser("y").complexRule.matched must beFalse
    }
  }
}