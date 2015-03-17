package org.parboiled2.examples

import org.parboiled2._

object SimpleLanguage {

  sealed abstract class AstNode

  sealed trait StatementNode extends AstNode

  case class SequenceNode(left: StatementNode, right: StatementNode) extends StatementNode

  sealed trait SequencedStatementNode extends StatementNode

  case class WhileNode(condition: ExpressionNode, body: StatementNode) extends SequencedStatementNode

  case class AssignNode(variableName: VariableNameNode, value: ExpressionNode) extends SequencedStatementNode

  case class VariableNameNode(name: String) extends ExpressionNode

  case class IfNode(condition: ExpressionNode, ifTrue: StatementNode, ifFalse: StatementNode) extends SequencedStatementNode

  case class DoNothingNode() extends SequencedStatementNode

  sealed trait ExpressionNode extends AstNode

  case class LessThanNode(left: ExpressionNode, right: ExpressionNode) extends ExpressionNode

  case class AddNode(left: ExpressionNode, right: ExpressionNode) extends ExpressionNode

  case class MultiplyNode(left: ExpressionNode, right: ExpressionNode) extends ExpressionNode

  case class BracketsNode(value: ExpressionNode) extends ExpressionNode

  sealed trait TermNode extends ExpressionNode

  case class NumberNode(number: Int) extends TermNode

  case class BoolNode(bool: scala.Boolean) extends TermNode

}

/**
 * A parser for the SIMPLE language defined by Tom Stuart for his book Understanding Computation
 * Ported from the Treetop grammar [here](https://github.com/tomstuart/computationbook/blob/master/the_meaning_of_programs/parser/simple.treetop)
 * It reads SIMPLE programs and parses them into an AST. See the companion object for the AST node definitions
 */

class SimpleLanguage(val input: ParserInput) extends Parser {

  import org.parboiled2.examples.SimpleLanguage._

  def Program: Rule1[StatementNode] = rule {
    Statement ~ EOI
  }

  def Statement: Rule1[StatementNode] = rule {
    Sequence /*~ EOI*/
  }

  def Sequence: Rule1[StatementNode] = rule {
    (SequencedStatement ~ "; " ~ Sequence ~> SequenceNode) | SequencedStatement
  }

  def SequencedStatement: Rule1[SequencedStatementNode] = rule {
    While | Assign | If | DoNothing
  }

  def While = rule {
    "while (" ~ Expression ~ ") { " ~ Statement ~ " }" ~> WhileNode
  }

  def Assign = rule {
    VariableName ~ " = " ~ Expression ~> AssignNode
  }

  def If: Rule1[SequencedStatementNode] = rule {
    "if (" ~ Expression ~ ") { " ~ Statement ~ " } else { " ~ Statement ~ " }" ~> IfNode
  }

  def DoNothing: Rule1[SequencedStatementNode] = rule {
    capture("do-nothing") ~> { (s: String) => DoNothingNode()}
  }

  def ExpressionLine: Rule1[ExpressionNode] = rule {
    Expression ~ EOI
  }

  def Expression: Rule1[ExpressionNode] = rule {
    LessThan
  }

  def LessThan: Rule1[ExpressionNode] = rule {
    (Add ~ " < " ~ LessThan ~> LessThanNode) | Add
  }

  def Add: Rule1[ExpressionNode] = rule {
    (Multiply ~ " + " ~ Add ~> AddNode) | Multiply
  }

  def Multiply: Rule1[ExpressionNode] = rule {
    (Brackets ~ " * " ~ Multiply ~> MultiplyNode) | Brackets
  }

  def Brackets: Rule1[ExpressionNode] = rule {
    ("(" ~ Expression ~ ")" ~> BracketsNode) | Term
  }

  def Term: Rule1[ExpressionNode] = rule {
    Number | Bool | Variable
  }

  def Number: Rule1[NumberNode] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((s: String) => NumberNode(s.toInt))
  }

  def Bool: Rule1[BoolNode] = rule {
    (capture("true") | capture("false")) ~> ((s: String) => BoolNode(s == "true"))
  }

  def Variable: Rule1[VariableNameNode] = rule {
    VariableName
  }

  def VariableName: Rule1[VariableNameNode] = rule {
    capture(oneOrMore(CharPredicate.LowerAlpha)) ~> VariableNameNode
  }
}
