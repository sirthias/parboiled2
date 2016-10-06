package org.parboiled2.examples

import org.specs2.mutable.Specification
import org.parboiled2.examples.SimpleLanguage._

class SimpleLanguageSpec extends Specification {
  "Parsing booleans" should {
    "work for 'true'" in {
      val parser = new SimpleLanguage("true")
      parser.Bool.run().isSuccess must beTrue
      parser.Bool.run().get must be equalTo BoolNode(true)
    }

    "work for 'false'" in {
      val parser = new SimpleLanguage("false")
      parser.Bool.run().isSuccess must beTrue
      parser.Bool.run().get must be equalTo BoolNode(false)
    }

    "fail on incorrect input" in {
      new SimpleLanguage("gibberish").Bool.run().isFailure must beTrue
    }

    "fail on empty input" in {
      new SimpleLanguage("").Bool.run().isFailure must beTrue
    }
  }

  "Parsing numbers" should {
    "work for single-digit numbers" in {
      val parser = new SimpleLanguage("7")
      parser.Number.run().isSuccess must beTrue
      parser.Number.run().get must beEqualTo(NumberNode(7))
    }

    "work for zero numbers" in {
      val parser = new SimpleLanguage("0")
      parser.Number.run().isSuccess must beTrue
      parser.Number.run().get must beEqualTo(NumberNode(0))
    }

    "work for multi-digit numbers" in {
      val parser = new SimpleLanguage("12345")
      parser.Number.run().isSuccess must beTrue
      parser.Number.run().get must beEqualTo(NumberNode(12345))
    }

    "fail on gibberish" in {
      val parser = new SimpleLanguage("aoeu")
      parser.Number.run().isFailure must beTrue
    }

    "fail on empty input" in {
      val parser = new SimpleLanguage("")
      parser.Number.run().isFailure must beTrue
    }
  }

  "Parsing variable names should" in {
    "work for single-character names" in {
      val parser = new SimpleLanguage("a")
      parser.VariableName.run().isSuccess must beTrue
      parser.VariableName.run().get must be equalTo VariableNameNode("a")
    }

    "work for multi-character names" in {
      val parser = new SimpleLanguage("myvariablename")
      parser.VariableName.run().isSuccess must beTrue
      parser.VariableName.run().get must be equalTo VariableNameNode("myvariablename")
    }

    /* limitation: the subrules don't match until end-of-input. They would fail when included in a bigger expression (Foo ~ VariableName ~ Bar ~ EOI) but this will return "the"
    "fail on variable names with spaces" in {
      val parser = new CompleteSimpleParser("my variable name")
      parser.VariableName.run().isSuccess must beFalse
    }*/

    "fail on empty input" in {
      val parser = new SimpleLanguage("")
      parser.VariableName.run().isSuccess must beFalse
    }

    "fail on numeric input" in {
      val parser = new SimpleLanguage("1234")
      parser.VariableName.run().isSuccess must beFalse
    }
  }

  "Parsing terms" should {
    "match numbers" in {
      val parser = new SimpleLanguage("12345")
      parser.Term.run().isSuccess must beTrue
      parser.Term.run().get must be equalTo NumberNode(12345)
    }

    "match variable names" in {
      val parser = new SimpleLanguage("myvariablename")
      parser.Term.run().isSuccess must beTrue
      parser.Term.run().get must be equalTo VariableNameNode("myvariablename")
    }

    "match booleans" in {
      val parser = new SimpleLanguage("true")
      parser.Term.run().isSuccess must beTrue
      parser.Term.run().get must be equalTo BoolNode(true)
    }

    /* Again, doesn't work because it doesn't match until EOI
    "not match expressions" in {
      val parser = new CompleteSimpleParser("1 + 1")
      parser.Term.run().isFailure must beTrue
    }*/
  }

  "Parsing brackets" should {
    "match booleans" in {
      val parser = new SimpleLanguage("(true)")
      parser.Brackets.run().isSuccess must beTrue
      parser.Brackets.run().get must be equalTo BracketsNode(BoolNode(true))
    }

    "match numbers" in {
      val parser = new SimpleLanguage("(12345)")
      parser.Brackets.run().isSuccess must beTrue
      parser.Brackets.run().get must be equalTo BracketsNode(NumberNode(12345))
    }

    "match variable names" in {
      val parser = new SimpleLanguage("(myvariablename)")
      parser.Brackets.run().isSuccess must beTrue
      parser.Brackets.run().get must be equalTo BracketsNode(VariableNameNode("myvariablename"))
    }

    "match expressions" in {
      "simple add expressions" in {
        val parser = new SimpleLanguage("(1 + 2)")
        parser.Brackets.run().isSuccess must beTrue
        parser.Brackets.run().get must be equalTo BracketsNode(AddNode(NumberNode(1), NumberNode(2)))
      }

      "simple multiplication expressions" in {
        val parser = new SimpleLanguage("(1 * 2)")
        parser.Brackets.run().isSuccess must beTrue
        parser.Brackets.run().get must be equalTo BracketsNode(MultiplyNode(NumberNode(1), NumberNode(2)))
      }

      "multiplication expressions with variables" in {
        val parser = new SimpleLanguage("(aoeu * 2)")
        parser.Brackets.run().isSuccess must beTrue
        parser.Brackets.run().get must be equalTo BracketsNode(MultiplyNode(VariableNameNode("aoeu"), NumberNode(2)))
      }

      "compound expressions" in {
        val parser = new SimpleLanguage("(aoeu * 2 + 1)")
        parser.Brackets.run().isSuccess must beTrue
        parser.Brackets.run().get must be equalTo BracketsNode(AddNode(MultiplyNode(VariableNameNode("aoeu"), NumberNode(2)), NumberNode(1)))
      }

      "less than expressions" in {
        val parser = new SimpleLanguage("(aoeu * 2 < 1)")
        parser.Brackets.run().isSuccess must beTrue
        parser.Brackets.run().get must be equalTo BracketsNode(LessThanNode(MultiplyNode(VariableNameNode("aoeu"), NumberNode(2)), NumberNode(1)))
      }
    }
  }

  "Parsing expressions" should {

    "match less-than expressions" in {
      "simple less-than expression" in {
        val parser = new SimpleLanguage("44 < 88")
        parser.LessThan.run().isSuccess must beTrue
        parser.LessThan.run().get must be equalTo LessThanNode(NumberNode(44), NumberNode(88))
      }

      "complex less-than expressions" in {
        val parser = new SimpleLanguage("44 + 8 * 3 < 88 * (77 + 2)")
        parser.LessThan.run().isSuccess must beTrue
        parser.LessThan.run().get must be equalTo LessThanNode(AddNode(NumberNode(44), MultiplyNode(NumberNode(8), NumberNode(3))), MultiplyNode(NumberNode(88), BracketsNode(AddNode(NumberNode(77), NumberNode(2)))))
      }

    }

    "match terms" in {
      "match numbers" in {
        val parser = new SimpleLanguage("12345")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo NumberNode(12345)
      }

      "match variable names" in {
        val parser = new SimpleLanguage("myvariablename")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo VariableNameNode("myvariablename")
      }

      "match booleans" in {
        val parser = new SimpleLanguage("true")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo BoolNode(true)
      }
    }

    "match bracketed expressions" in {
      "simple add expressions" in {
        val parser = new SimpleLanguage("(1 + 2)")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo BracketsNode(AddNode(NumberNode(1), NumberNode(2)))
      }

      "simple multiplication expressions" in {
        val parser = new SimpleLanguage("(1 * 2)")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo BracketsNode(MultiplyNode(NumberNode(1), NumberNode(2)))
      }

      "multiplication expressions with variables" in {
        val parser = new SimpleLanguage("(aoeu * 2)")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo BracketsNode(MultiplyNode(VariableNameNode("aoeu"), NumberNode(2)))
      }

      "compound expressions" in {
        val parser = new SimpleLanguage("(aoeu * 2 + 1)")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo BracketsNode(AddNode(MultiplyNode(VariableNameNode("aoeu"), NumberNode(2)), NumberNode(1)))
      }

      "less than expressions" in {
        val parser = new SimpleLanguage("(aoeu * 2 < 1)")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo BracketsNode(LessThanNode(MultiplyNode(VariableNameNode("aoeu"), NumberNode(2)), NumberNode(1)))
      }
    }

    "match add expressions" in {
      "simple sums" in {
        val parser = new SimpleLanguage("5 + 7")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo AddNode(NumberNode(5), NumberNode(7))
      }

      "sums with multiple terms" in {
        val parser = new SimpleLanguage("5 + 7 + 3 + 2")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo AddNode(NumberNode(5), AddNode(NumberNode(7), AddNode(NumberNode(3), NumberNode(2))))
      }

      "sums with brackets" in {
        val parser = new SimpleLanguage("5 + (7 + 3) + 2")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo AddNode(NumberNode(5), AddNode(BracketsNode(AddNode(NumberNode(7), NumberNode(3))), NumberNode(2)))
      }

      "sums with a multiplication in them" in {
        val parser = new SimpleLanguage("5 + 7 * 3 + 2")
        parser.Expression.run().isSuccess must beTrue
        parser.Expression.run().get must be equalTo AddNode(NumberNode(5), AddNode(MultiplyNode(NumberNode(7), NumberNode(3)), NumberNode(2)))
      }
    }
  }

  "Parsing add expressions" should {
    "match simple add expressions" in {
      val parser = new SimpleLanguage("7 + 12")
      parser.Add.run().isSuccess must beTrue
      parser.Add.run().get must be equalTo AddNode(NumberNode(7), NumberNode(12))
    }

    "match simple add expressions with multiple terms" in {
      val parser = new SimpleLanguage("7 + 12 + 2 + 3")
      parser.Add.run().isSuccess must beTrue
      parser.Add.run().get must be equalTo AddNode(NumberNode(7), AddNode(NumberNode(12), AddNode(NumberNode(2), NumberNode(3))))
    }

    "match add expressions with brackets" in {
      val parser = new SimpleLanguage("7 + (12 + 2) + 3")
      parser.Add.run().isSuccess must beTrue
      parser.Add.run().get must be equalTo AddNode(NumberNode(7), AddNode(BracketsNode(AddNode(NumberNode(12), NumberNode(2))), NumberNode(3)))
    }
  }

  "Parsing multiplication expressions" should {
    "Match simple multiplication expressions" in {
      val parser = new SimpleLanguage("7 * 12")
      parser.Add.run().isSuccess must beTrue
      parser.Add.run().get must be equalTo MultiplyNode(NumberNode(7), NumberNode(12))
    }

    "match simple multiplication expressions with multiple terms" in {
      val parser = new SimpleLanguage("7 * 12 * 2 * 3")
      parser.ExpressionLine.run().isSuccess must beTrue
      parser.ExpressionLine.run().get must be equalTo MultiplyNode(NumberNode(7), MultiplyNode(NumberNode(12), MultiplyNode(NumberNode(2), NumberNode(3))))
    }

    "match multiplication expressions with brackets" in {
      val parser = new SimpleLanguage("7 * (12 * 2) * 3")
      parser.ExpressionLine.run().isSuccess must beTrue
      parser.ExpressionLine.run().get must be equalTo MultiplyNode(NumberNode(7), MultiplyNode(BracketsNode(MultiplyNode(NumberNode(12), NumberNode(2))), NumberNode(3)))
    }

    "match multiplication expressions with add expression" in {
      val parser = new SimpleLanguage("7 * 12 + 2 * 3")
      parser.ExpressionLine.run().isSuccess must beTrue
      parser.ExpressionLine.run().get must be equalTo AddNode(MultiplyNode(NumberNode(7), NumberNode(12)), MultiplyNode(NumberNode(2), NumberNode(3)))
    }
  }

  "Parsing less-than expressions" should {
    "work for simple literals" in {
      val parser = new SimpleLanguage("44 < 88")
      parser.LessThan.run().isSuccess must beTrue
      parser.LessThan.run().get must be equalTo LessThanNode(NumberNode(44), NumberNode(88))
    }

    "work for complex expressions" in {
      val parser = new SimpleLanguage("44 + 8 * 3 < 88 * (77 + 2)")
      parser.LessThan.run().isSuccess must beTrue
      parser.LessThan.run().get must be equalTo LessThanNode(AddNode(NumberNode(44), MultiplyNode(NumberNode(8), NumberNode(3))), MultiplyNode(NumberNode(88), BracketsNode(AddNode(NumberNode(77), NumberNode(2)))))
    }
  }

  "Parsing do-nothing statements" should {
    "match do-nothing" in {
      val parser = new SimpleLanguage("do-nothing")
      parser.DoNothing.run().isSuccess must beTrue
      parser.DoNothing.run().get must be equalTo DoNothingNode()
    }

    "fail on gibberish" in {
      val parser = new SimpleLanguage("gibberish")
      parser.DoNothing.run().isFailure must beTrue
    }
  }

  "Parsing if statements" should {
    "match simple if statements" in {
      val parser = new SimpleLanguage("if (true) { do-nothing } else { do-nothing }")
      parser.If.run().isSuccess must beTrue
      parser.If.run().get must be equalTo IfNode(BoolNode(true), DoNothingNode(), DoNothingNode())
    }

    "match complex if statements" in {
      val parser = new SimpleLanguage("if (88 * 5 < 2 + 3) { do-nothing; do-nothing } else { do-nothing; aoeu = 4; do-nothing }")
      parser.If.run().isSuccess must beTrue
      parser.If.run().get must be equalTo IfNode(
        LessThanNode(MultiplyNode(NumberNode(88), NumberNode(5)), AddNode(NumberNode(2), NumberNode(3))),
        SequenceNode(DoNothingNode(), DoNothingNode()),
        SequenceNode(DoNothingNode(), SequenceNode(AssignNode(VariableNameNode("aoeu"), NumberNode(4)), DoNothingNode()))
      )
    }
  }

  "Parsing assign statements" should {
    "match simple assignment" in {
      val parser = new SimpleLanguage("somevar = 88")
      parser.Assign.run().isSuccess must beTrue
      parser.Assign.run().get must be equalTo AssignNode(VariableNameNode("somevar"), NumberNode(88))
    }

    "match complex assignment" in {
      val parser = new SimpleLanguage("somevar = (7 + 8 * 11)")
      parser.Assign.run().isSuccess must beTrue
      parser.Assign.run().get must be equalTo AssignNode(VariableNameNode("somevar"), BracketsNode(AddNode(NumberNode(7), MultiplyNode(NumberNode(8), NumberNode(11)))))
    }

    "match variable to variable assignment" in {
      val parser = new SimpleLanguage("somevar = anothervar")
      parser.Assign.run().isSuccess must beTrue
      parser.Assign.run().get must be equalTo AssignNode(VariableNameNode("somevar"), VariableNameNode("anothervar"))
    }
  }

  "Parsing while statements" should {
    "match simple while loop" in {
      val parser = new SimpleLanguage("while (true) { aoeu = 4 }")
      parser.While.run().isSuccess must beTrue
      parser.While.run().get must be equalTo WhileNode(BoolNode(true), AssignNode(VariableNameNode("aoeu"), NumberNode(4)))
    }

    "match complex while loop" in {
      val parser = new SimpleLanguage("while (44 + 6 < 4) { aoeu = 4; snth = aoeu + 4 }")
      parser.While.run().isSuccess must beTrue
      parser.While.run().get must be equalTo WhileNode(
        LessThanNode(AddNode(NumberNode(44), NumberNode(6)), NumberNode(4)),
        SequenceNode(AssignNode(VariableNameNode("aoeu"), NumberNode(4)),
          AssignNode(VariableNameNode("snth"), AddNode(VariableNameNode("aoeu"), NumberNode(4)))
        )
      )
    }
  }

  "Parsing sequenced statements" should {
    "match if statement" in {
      val parser = new SimpleLanguage("if (true) { do-nothing } else { do-nothing }")
      parser.SequencedStatement.run().isSuccess must beTrue
      parser.SequencedStatement.run().get must be equalTo IfNode(BoolNode(true), DoNothingNode(), DoNothingNode())
    }

    "match assign statement" in {
      val parser = new SimpleLanguage("somevar = 88")
      parser.SequencedStatement.run().isSuccess must beTrue
      parser.SequencedStatement.run().get must be equalTo AssignNode(VariableNameNode("somevar"), NumberNode(88))
    }

    "match while statement" in {
      val parser = new SimpleLanguage("while (true) { aoeu = 4 }")
      parser.SequencedStatement.run().isSuccess must beTrue
      parser.SequencedStatement.run().get must be equalTo WhileNode(BoolNode(true), AssignNode(VariableNameNode("aoeu"), NumberNode(4)))
    }

    "match do-nothing" in {
      val parser = new SimpleLanguage("do-nothing")
      parser.SequencedStatement.run().isSuccess must beTrue
      parser.SequencedStatement.run().get must be equalTo DoNothingNode()
    }
  }

  "Parsing statements" should {
    "match single statements" in {
      val parser = new SimpleLanguage("do-nothing")
      parser.Statement.run().isSuccess must beTrue
      parser.Statement.run().get must be equalTo DoNothingNode()
    }

    "match multiple statements" in {
      val parser = new SimpleLanguage("do-nothing; do-nothing")
      parser.Statement.run().isSuccess must beTrue
      parser.Statement.run().get must be equalTo SequenceNode(DoNothingNode(), DoNothingNode())
    }
  }

  "Parsing programs" should {
    "match single statements" in {
      val parser = new SimpleLanguage("do-nothing")
      parser.Program.run().isSuccess must beTrue
      parser.Program.run().get must be equalTo DoNothingNode()
    }

    "match multiple statements" in {
      val parser = new SimpleLanguage("do-nothing; do-nothing")
      parser.Program.run().isSuccess must beTrue
      parser.Program.run().get must be equalTo SequenceNode(DoNothingNode(), DoNothingNode())
    }
  }

  "Parsing operator precedence" in {
    "Addition has precedence over less than" in {
      val parser = new SimpleLanguage("1 + 2 < 3")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo LessThanNode(AddNode(NumberNode(1), NumberNode(2)), NumberNode(3))
    }

    "Brackets have precedence over addition" in {
      val parser = new SimpleLanguage("1 + (2 < 3)")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo AddNode(NumberNode(1), BracketsNode(LessThanNode(NumberNode(2), NumberNode(3))))
    }

    "Addition has precedence over less than (bis)" in {
      val parser = new SimpleLanguage("1 < 2 + 3")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo LessThanNode(NumberNode(1), AddNode(NumberNode(2), NumberNode(3)))
    }

    "Less than has precedence over addition when surrounded by brackets" in {
      val parser = new SimpleLanguage("(1 < 2) + 3")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo AddNode(BracketsNode(LessThanNode(NumberNode(1), NumberNode(2))), NumberNode(3))
    }

    "Multiplication has precedence over addition" in {
      val parser = new SimpleLanguage("1 + 2 * 3")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo AddNode(NumberNode(1), MultiplyNode(NumberNode(2), NumberNode(3)))
    }

    "Multiplication has precedence over addition, which has precedence over less than" in {
      val parser = new SimpleLanguage("1 + 2 < 3 * 4")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo LessThanNode(AddNode(NumberNode(1), NumberNode(2)), MultiplyNode(NumberNode(3), NumberNode(4)))
    }

    "Brackets have predence over multiplication" in {
      val parser = new SimpleLanguage("(1 + 2) * 3")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo MultiplyNode(BracketsNode(AddNode(NumberNode(1), NumberNode(2))), NumberNode(3))
    }

    "Multiplication has precedence over addition" in {
      val parser = new SimpleLanguage("1 * 2 + 3")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo AddNode(MultiplyNode(NumberNode(1), NumberNode(2)), NumberNode(3))
    }

    "Brackets have predence over multiplication (bis)" in {
      val parser = new SimpleLanguage("1 * (2 + 3)")
      parser.Expression.run().isSuccess must beTrue
      parser.Expression.run().get must be equalTo MultiplyNode(NumberNode(1), BracketsNode(AddNode(NumberNode(2), NumberNode(3))))
    }
  }
}
