/*
 * Copyright (C) 2009-2013 Mathias Doenitz, Alexander Myltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.parboiled2.examples.v2

abstract class JSonTerm
case class JSonObject(members: Seq[JSonPair]) extends JSonTerm
case class JSonArray(elements: Seq[JSonTerm]) extends JSonTerm
case class JSonPair(str: String, value: JSonTerm) extends JSonTerm
case class JSonString(str: String) extends JSonTerm
case class JSonNumber(num: Int) extends JSonTerm
case class JSonBoolean(f: Boolean) extends JSonTerm

import org.parboiled2._
import scala.annotation.tailrec
import shapeless._

class JsonSimpleParser(val input: ParserInput) extends Parser {
  def InputLine = rule { Object ~ EOI }

  def Object = rule {
    ("{" ~ optional(Members) ~ "}") ~>
      ((x: Option[List[JSonPair]]) ⇒ JSonObject(x.getOrElse(List())))
  }

  // NOTE: Both are valid. Second one is using `ReductionRule`
  // TODO: Figure out why type errors
  //def Members: Rule1[List[JSonPair]] = rule { Pair ~ zeroOrMore("," ~ Pair) ~> ((x: JSonPair, y: List[JSonPair]) => x :: y) }
  def Members: Rule1[List[JSonPair]] = rule { (Pair ~> (List(_))) ~ zeroOrMore("," ~ Pair ~> ((_: List[JSonPair]) :+ _)) }

  // NOTE: Both are valid
  def Pair = rule { String ~> ((x: JSonString) ⇒ x.str) ~ ":" ~ Value ~> (JSonPair((_: String), (_: JSonTerm))) }
  //def Pair: Rule1[JSonPair] = rule { String ~ ":" ~ Value ~> ((x: JSonString, t: JSonTerm) => JSonPair(x.str, t)) }

  def Array: Rule1[JSonArray] = rule {
    "[" ~ Value ~ zeroOrMore("," ~ Value) ~>
      ((x: JSonTerm, y: Seq[JSonTerm]) ⇒ JSonArray(x +: y)) ~ "]"
  }

  def String = rule { "\"" ~ capture(zeroOrMore(Char)) ~> JSonString ~ "\"" }

  def Char = rule { "a" - "z" | "A" - "Z" | Digit }

  def Value: Rule1[JSonTerm] = rule { String | Number | Array | Boolean | Object }

  // NOTE: Two ways to push result of `("true" | "false")` matching
  def Boolean: Rule1[JSonTerm] = rule {
    (str("true") ~ push(true) ~> JSonBoolean) |
      (capture("false") ~> ((x: String) ⇒ JSonBoolean(false)))
  }

  def Number = rule { capture(Integer) ~> ((x: String) ⇒ JSonNumber(x.toInt)) }

  def Integer = rule { optional("-") ~ (("1" - "9") ~ zeroOrMore(Digit) | oneOrMore(Digit)) }

  def Digit = rule { "0" - "9" }
}

object JsonSimpleParser {
  private def ind(indent: Int) = "  " * indent

  def prettyPrint(jt: JSonTerm, indent: Int = 0): String = jt match {
    case JSonObject(mbrs) ⇒
      mbrs.map(prettyPrint(_, indent + 1)).mkString("{\n", ",\n", "\n" + ind(indent) + "}")
    case JSonPair(string, value) ⇒ ind(indent) + "\"" + string + "\": " + prettyPrint(value, indent + 1)
    case JSonString(str)         ⇒ s"""\"${str}\""""
    case JSonNumber(num)         ⇒ num.toString
    case JSonArray(elms)         ⇒ s"[${elms.map(prettyPrint(_, indent + 1)).mkString(", ")}]"
    case JSonBoolean(f)          ⇒ s"$f"
  }

  @tailrec
  def repl(): Unit = {
    val inputLine = readLine("--------------------------------------\nEnter expression for JSon simple parser (v2) > ")
    if (inputLine != "") {
      val jsonSimpleParser = new JsonSimpleParser(inputLine)
      jsonSimpleParser.run(_.InputLine) match {
        case Right(x)  ⇒ println(s"Expression is valid. Result: ${x}. Pretty-printed: ${prettyPrint(x.head)}")
        case Left(err) ⇒ println(s"Expression is not valid. Error: ${ErrorUtils.formatError(inputLine, err)}\n")
      }
      repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}