package calculators.v2

import org.parboiled2._
import scala.annotation.tailrec
import org.parboiled.scala.parserunners.ReportingParseRunner

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
    print("--------------------------------------\n")
    print("Enter expression for calculator (v2) > ")
    val inputLine = readLine()
    if (inputLine != "") {
      val simpleCalc = new SimpleCalculator(inputLine)
      if (simpleCalc.InputLine.matched) {
        println("Expression is valid")
      } else {
        val errUtils = new ErrorUtils {
          def parser = simpleCalc
          def targetRule = simpleCalc.InputLine
        }
        println(s"[v2] Expression is not valid. Error: ${errUtils.parseErrors}")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}
