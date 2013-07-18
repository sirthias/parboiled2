package calculators.v2

import org.parboiled2._
import scala.annotation.tailrec

class SimpleCalculator(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule = rule { Term ~ zeroOrMore((ch('+') | '-') ~ Term) }

  def Term = rule { Factor ~ zeroOrMore((ch('*') | '/') ~ Factor) }

  def Factor = rule { Digits | Parens }

  def Parens = rule { "(" ~ Expression ~ ")" }

  def Digits = rule { oneOrMore(Digit) }

  def Digit = rule { (ch('0') | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') }
}

object CalculatorExpressionVerifier {
  @tailrec
  def repl(): Unit = {
    print("Enter expression for calculator > ")
    val inputLine = readLine()
    if (inputLine != "") {
      val simpleCalc = new SimpleCalculator(inputLine)
      if (simpleCalc.InputLine.matched) {
        println("Expression is valid")
      } else {
        println(s"Expression is not valid. Errors: ${simpleCalc.errors() mkString "\n"}")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}