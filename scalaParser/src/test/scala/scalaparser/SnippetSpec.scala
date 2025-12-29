/*
 * Copyright 2009-2019 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scalaparser

import scala.util.{Failure, Success}
import utest.*
import org.parboiled2.*

object SnippetSpec extends TestSuite {

  val tests = Tests {

    "The ScalaParser should properly parse the positive example snippets" - {

      "example 001" - {
        check("package torimatomeru")
      }

      "example 002" - {
        check("""
            |package torimatomeru
            |
            |package lols""")
      }

      "example 003" - {
        check("""package torimatomeru
            |import a
            |import b
          """)
      }

      "example 004" - {
        check("""package torimatomeru
            |
            |import org.parboiled2.ParseError
            |import utest._
            |import utest.framework.Test
            |import utest.util.Tree
            |
            |import scala.util.{Failure, Success}
            |
            |object SyntaxTest extends TestSuite""")
      }

      "example 005" - {
        check("""object SyntaxTest extends TestSuite{
            |  def check[T](input: String) = {
            |
            |  }
            |}""")
      }

      "example 006" - {
        check("""object SyntaxTest{
            |  a()
            |  throw 1
            |}""")
      }

      "example 007" - {
        check("""object SyntaxTest extends TestSuite{
            |  {
            |        println
            |        throw 1
            |  }
            |}""")
      }

      "example 002" - {
        check("""package scalatex
            |
            |
            |import org.parboiled2._
            |import torimatomeru.ScalaSyntax
            |
            |import scalatex.stages.{Trim, Parser, Ast}
            |import scalatex.stages.Ast.Block.{IfElse, For, Text}
            |import Ast.Chain.Args
            |
            |object ParserTests extends utest.TestSuite{
            |  import Ast._
            |  import utest._
            |  def check[T](input: String, parse: Parser => scala.util.Try[T], expected: T) = {
            |    val parsed = parse(new Parser(input)).get
            |    assert(parsed == expected)
            |  }
            |  def tests = TestSuite{}
            |}""")
      }

      "example 009" - {
        check("""object Moo{
            |  a
            |  .b
            |
            |  c
            |}""")
      }

      "example 010" - {
        check("""object Moo{
            | filename
            |        .asInstanceOf[Literal]
            |10
            |}""")
      }

      "example 011" - {
        check("""object Cow{
            |  ().mkString
            |
            |  1
            |}""")
      }

      "example 012" - {
        check("""object O{
            | private[this] val applyMacroFull = 1
            |}""")
      }

      "example 013" - {
        check("""object O{
            | private[this] def applyMacroFull(c: Context)
            |                      (expr: c.Expr[String],
            |                       runtimeErrors: Boolean,
            |                       debug: Boolean)
            |                      : c.Expr[Frag] = {
            |                      }
            |}""")
      }

      "example 014" - {
        check("""object O{
            |  class DebugFailure extends Exception
            |
            |  1
            |}""")
      }

      "example 015" - {
        check("""package torimatomeru
            |
            |package syntax
            |
            |import org.parboiled2._
            |
            |""")
      }

      "example 016" - {
        check("""object Foo{
            |  0 match {
            |    case A | B => 0
            |  }
            |}""")
      }

      "example 017" - {
        check("""object Compiler{
            |
            |  def apply = {
            |    def rec = t match {
            |      case 0 => 0
            |    }
            |
            |    rec(tree)
            |  }
            |}""")
      }

      "example 018" - {
        check("""object O {
            |    A(A(A(A(A(A(A(A())))))))
            |}""")
      }

      "example 019" - {
        check("""object O{
            |   A(A(A(A(A(A(A(A(A(A(A(A(A(A(A(A())))))))))))))))
            |}""")
      }

      "example 020" - {
        check("""object L{
            |  a.b = c
            |  a().b = c
            |}      """)
      }

      "example 021" - {
        check("""object L{
            |  a b c
            |  d = 1
            |}""")
      }

      "example 022" - {
        check("""/*                     __                                               *\
            |**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
            |**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
            |**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
            |** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
            |**                          |/____/                                     **
            |\*                                                                      */
            |
            |package scala.scalajs.cli
            |""")
      }

      "example 023" - {
        check("""object O{
            |  for {
            |      a  <- b
            |      c <- d
            |  } {
            |    1
            |  }
            |}""")
      }

      "example 024" - {
        check("""object O{
            |  val jarFile =
            |      try { 1 }
            |      catch { case _: F => G }
            |}""")
      }

      "example 025" - {
        check("""object F{
            |  func{ case _: F => fail }
            |}""")
      }

      "example 026" - {
        check("""object Foo{
            |    val a = d // g
            |    val b = e // h
            |    val c = f
            |}""")
      }

      "example 027" - {
        check("""object L{
            |  x match{
            |    case y.Y(z) => z
            |  }
            |}""")
      }

      "example 028" - {
        check("""object K{
            |  val a: B {
            |    val c: D
            |  }
            |
            |  1
            |}""")
      }

      "example 029" - {
        check("""object LOLS{
            |    def run() {}
            |
            |    def apply() {}
            |}""")
      }

      "example 030" - {
        check("""object O{
            |  a =:= b.c
            |}""")
      }

      "example 031" - {
        check("""object K{
            |  a(
            |    1: _*
            |  )
            |}""")
      }

      "example 032" - {
        check("""object P{
            |      tree match {
            |        case stats :+ expr  => 1
            |      }
            |}""")
      }

      "example 033" - {
        check("""object K{
            |  val trueA = 1
            |}""")
      }

      "example 034" - {
        check("""object K{
            |  val nullo :: cow = 1
            |}""")
      }

      "example 035" - {
        check("""object K{
            |  val omg_+ = 1
            |}""")
      }

      "example 036" - {
        check("""object K{
            |  val + = 1
            |  var * = 2
            |}""")
      }

      "example 037" - {
        check("""object O{
            |  c match {
            |    case b_  => 1
            |  }
            |}""")
      }

      "example 038" - {
        check("""trait Basic {
            |  b match {
            |    case C => true; case _ => false
            |  }
            |}""")
      }

      "example 039" - {
        check("""trait Basic {
            |  !a.b
            |}""")
      }

      "example 040" - {
        check("""class Parser {
            |  {() => }
            |}
            |""")
      }

      "example 040" - {
        check("""
            |
            |
            |
            |package omg
            |;
            |
            |;
            |
            |;
            |class Parser
            |;
            |
            |;
            |
            |;
          """)
      }

      "example 042" - {
        check("""object GenJSCode {
            |  code: @switch
            |}""")
      }

      "example 043" - {
        check("""object B {
            |  { a: L => }
            |}""")
      }

      "example 044" - {
        check("""object O{
            |  {
            |    val index = 0
            |    i: Int => 10
            |    0
            |  }
            |}""")
      }

      "example 045" - {
        check("""object GenJSCode{
            |  val g: G.this.g.type
            |}
            |""")
      }

      "example 046" - {
        check("""object K{
            |  class RTTypeTest
            |  private object O
            |}""")
      }

      "example 047" - {
        check("""object O{
            |  if (eqeq &&
            |
            |    false)  1
            |}
          """)
      }

      "example 048" - {
        check("""object O{
            |  for(
            |    x <- Nil map
            |
            |  (x => x)
            |  ) yield x
            |}""")
      }

      "example 049" - {
        check("""object O{
            |  for{
            |    x <- Nil
            |    if
            |
            |    1 == 2
            |  } yield x
            |}""")
      }

      "example 050" - {
        check("""object ScopedVar {
            |  def withScopedVars(ass: Seq[_]) = 1
            |}""")
      }

      "example 051" - {
        check("""abstract class JSASTTest extends DirectTest {
            |  def show: this.type = ()
            |}""")
      }

      "example 052" - {
        check("""object Traversers {
            |  {
            |        1
            |        cases foreach nil
            |  }
            |}""")
      }

      "example 053" - {
        check("""object Utils {
            |  "\\"
            |}
            |""")
      }

      "example 054" - {
        check("""object F{
            |  this eq that.asInstanceOf[AnyRef]
            |}""")
      }

      "example 055" - {
        check("""class C{
            |  0x00 <= 2 && 1
            |}
            |""")
      }

      "example 056" - {
        check("""class Runtime private
          """)
      }

      "example 057" - {
        check("""object System {
            |  def a[@b T[@b V]] = 1
            |}
            |""")
      }

      "example 058" - {
        check("""object U{
            |  private val _fragment = fld(Fragment)
            |  _fld = null
            |}""")
      }

      "example 059" - {
        check("""class Array{
            |  def length_= = 1
            |}""")
      }

      "example 060" - {
        check("""object K{
            |  def newBuilder =
            |    new B
            |
            |  @inline def a = 1
            |}""")
      }

      "example 061" - {
        check("""trait Function12[-T1, +R]
          """)
      }

      "example 062" - {
        check("""@a // Don't do this at home!
            |trait B
          """)
      }

      "example 063" - {
        check("""object T{
            |  type B = { def F: S }
            |}
            |""")
      }

      "example 064" - {
        check("""
            |object ScalaJSBuild{
            |      (
            |        1 / 2
            |          / 3
            |      )
            |}""")
      }

      "example 065" - {
        check("""trait Writer{
            | '\f'
            |}
          """)
      }

      "example 066" - {
        check("""object CyclicDependencyException {
            |    def str(info: ResolutionInfo) =
            |      s"${info.resourceName} from: ${info.origins.mkString(", ")}"
            |}""")
      }

      "example 067" - {
        check("""object OptimizerCore {
            |  tpe match {
            |    case NothingType | _:RecordType=> 1
            |  }
            |}""")
      }

      "example 068" - {
        check("""class A{
            |  1
            |  () => 1
            |}""")
      }

      "example 069" - {
        check("""trait ReactorCanReply {
            |  _: InternalReplyReactor =>
            |}""")
      }

      "example 070" - {
        check("""object G{
            |  def isBefore(pd: SubComponent) = settings.stopBefore
            |  phaseDescriptors sliding 2 collectFirst ()
            |}""")
      }

      "example 071" - {
        check("""class SymbolLoaders {
            |  type T = ClassPath[AbstractFile]#ClassRep
            |}""")
      }

      "example 072" - {
        check("""trait ContextErrors {
            |    def isUnaffiliatedExpr = expanded.isInstanceOf[scala.reflect.api.Exprs#Expr[_]]
            |}""")
      }

      "example 073" - {
        check("""trait Typers{
            |  s"nested ${ if (1) "trait" else "class" }"
            |}""")
      }

      "example 074" - {
        check("""trait ReflectSetup { this: Global =>
            |  phase = 1
            |}""")
      }

      "example 075" - {
        check("""trait Predef {
            |  @x
            |  // a
            |  type T
            |}""")
      }

      "example 076" - {
        check("""object StringContext {

              s"${
                require(index >= 0 && index < str.length)
                val ok = "[\b, \t, \n, \f, \r, \\, \", \']"
                if (index == str.length - 1) "at terminal" else s"'\\${str(index + 1)}' not one of $ok at"
              }"

            }
        """)
      }

      "example 077" - {
        check("""trait Growable {
            |    +=
            |}""")
      }

      "example 078" - {
        check("""package immutable {
            |  object O
            |}""")
      }

      "example 079" - {
        check("""import java.util.concurrent.TimeUnit.{ NANOSECONDS => NANOS, MILLISECONDS => MILLIS }
          """)
      }

      "example 080" - {
        check("""class FunFinder{
            |  val targetName = s"$name${ if (isModule) "$" else "" }"
            |}""")
      }

      "example 081" - {
        check("""class AkkaException{
            |  for (i <- 0 until trace.length)
            |    ()
            |}""")
      }

      "example 082" - {
        check("""object Test4 {
            |    type T = F @field
            |    @BeanProperty val x = 1
            |}""")
      }

      "example 083" - {
        check("""package `dmacro` {
            |}""")
      }

      "example 084" - {
        check("""class A {
            |  def fn1 = List apply 1
            |  def fn2 = List apply[Int] 2
            |}""")
      }

      "example 085" - {
        check("""class C {
            |  def this(x: Int) = {
            |    this();
            |    class D;
            |  }
            |}""")
      }

      "example 086" - {
        check("""trait B[T] {
            |  def f1(a: T): Unit { }
            |}""")
      }

      "example 087" - {
        check("""object test {
            |  case object Int16 extends SampleFormat1
            |  (1) match {
            |    case _   => 1
            |  }
            |}""")
      }

      "example 088" - {
        check("""object A {
            |  def x {
            |    implicit lazy val e: Int = 0
            |  }
            |}""")
      }

      "example 089" - {
        check("""object test {
            |  for {
            |    n <- A
            |    a <- B
            |    _ <- C
            |  } yield n
            |}""")
      }

      //        check(
      //          """object Test {
      //            |  def t1: M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[Inty @unchecked]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = x
      //            |}
      //          """
      //        }

      "example 090" - {
        check("""abstract class Mix___eFoo___wBar_I_ extends Foo___ with Bar_I_    { ; ; f; }
          """)
      }

      "example 091" - {
        check("""package test2 {
            |object N1M0;
            |}""")
      }

      "example 092" - {
        check("""class IP extends {
            |  val baz = "bar";
            |} with Foo(() => baz);""")
      }

      "example 093" - {
        check("""object Test extends App {
            |  val x: C {} = 1
            |}""")
      }

      "example 094" - {
        check("""trait LensFunctions {
            |  type T = A @> B
            |}""")
      }

      "example 095" - {
        check("""object ContravariantCoyonedaUsage {
            |  (schwartzian[Vector[String], ccord.I]
            |      (unstructuredData)(v => ccord.k(v(i)))(ccord.fi))
            |}""")
      }

      "example 096" - {
        check("""object MapTest{
            |  forAll { a: Int ==>> Int =>
            |  }
            |}""")
      }

      "example 097" - {
        check("""object Test {
            |  def countingDownActor = {
            |    val ms = 1
            |    (m: Int) =>
            |      val x = 1
            |      1
            |  }
            |}""")
      }

      "example 098" - {
        check("""object X{
            |  type T = {
            |    ;;;
            |    type x = Int
            |    ;;;
            |    type y = Int
            |    ;;;
            |  }
            |}""")
      }

      "example 099" - {
        check("""object X{
            |  <div />
            |}""")
      }

      "example 100" - {
        check("""object X{
            |  <div id="hello" />
            |}""")
      }

      "example 101" - {
        check("""object X{
            |  <url>https://github.com/lihaoyi/scalatags</url>
            |}""")
      }

      "example 102" - {
        check("""object X{
            |  <url>{ ;;;1 + 1 }</url>
            |}""")
      }

      "example 103" - {
        check("""object UserAgentCalculator extends Factory {
            |    for {
            |      userAgent <- userAgent
            |      findResult = ieMatch.find if findResult
            |    } yield ver
            |}""")
      }

      //        check(
      //          """class FunctionalBuilder{
      //            |  a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21), a22)
      //            |}
      //          """
      //        }

      "example 104" - {
        check("""class HtmlPage {
            |  <meta http-equiv="content-type" content={ 1 }/>
            |} """)
      }

      "example 105" - {
        check("""object K{
            |  <script> {{</script>
            |}""")
      }

      "example 106" - {
        check("""object O{
            |  e match { case <title>{ _* }</title> => }
            |}
            |""")
      }

      "example 107" - {
        check("""object Publish {
            |  val x =
            |    <inceptionYear>2009</inceptionYear>
            |
            |
            |
            |      <scm>
            |        <url>git://github.com/akka/akka.git</url>
            |        <connection>scm:git:git@github.com:akka/akka.git</connection>
            |      </scm>
            |} """)
      }

      "example 108" - {
        check("""object K{
            |    <foo baz="&amp;dog"/>
            |}""")
      }

      "example 109" - {
        check("""object X{
            |   pomExtra :=
            |      <url>https://github.com/lihaoyi/scalatags</url>
            |        <licenses>
            |          <license>
            |            <name>MIT license</name>
            |            <url>http://www.opensource.org/licenses/mit-license.php</url>
            |          </license>
            |        </licenses>
            |        <scm>
            |          <url>git://github.com/lihaoyi/scalatags.git</url>
            |          <connection>scm:git://github.com/lihaoyi/scalatags.git</connection>
            |        </scm>
            |        <developers>
            |          <developer>
            |            <id>lihaoyi</id>
            |            <name>Li Haoyi</name>
            |            <url>https://github.com/lihaoyi</url>
            |          </developer>
            |        </developers>
            |}
          """)
      }
    }

    "The ScalaParser should properly parse the negative example snippets" - {
      "example 500" - {
        checkError(
          "packge torimatomeru",
          """Invalid input 'g', expected 'a' (line 1, column 5):
            |packge torimatomeru
            |    ^
            |
            |3 rules mismatched at error location:
            |  /CompilationUnit/ /Body/ |:-4 /TopPackageSeq/ +:-4 /Key/ /RawKey/ "package":-4 / 'a'
            |  /CompilationUnit/ /Body/ |:-4 /TopStatSeq/ +:-4 /TopStat/ |:-4 /PkgBlock/ /Key/ /RawKey/ "package":-4 / 'a'
            |  /CompilationUnit/ /Body/ |:-4 /TopStatSeq/ +:-4 /TopStat/ |:-4 /PkgObj/ /Key/ /RawKey/ "package":-4 / 'a'
            |""".stripMargin
        )
      }
      //    checkError("110",
      //      """object O{
      //        |  for{
      //        |    x <- Nil map
      //        |
      //        |  (x => x)
      //        |  } yield x
      //        |}""",
      //    """
      //      |
      //    """)
      //
      //    checkError("111",
      //      """object O{
      //        |  for{
      //        |    x <- Nil
      //        |    if 1 ==
      //        |
      //        |    2
      //        |  } yield x
      //        |}""")
      //
      //    checkError("112",
      //      """object O{
      //        |  for{
      //        |    x <- Nil
      //        |    _ = 1 ==
      //        |
      //        |    2
      //        |  } yield x
      //        |}""")
      //
      //    checkError("113",
      //      """object System {
      //        |  def a[@b T[V @b]] = 1
      //        |}
      //        |""")
    }
  }

  val formatter = new ErrorFormatter(showTraces = true)

  def fail(msg: Any): Unit = Predef.assert(false, msg)

  def check(snippet: String): Unit = {
    val parser = new ScalaParser(snippet.stripMargin)
    parser.CompilationUnit.run() match {
      case Failure(error: ParseError) => fail(error.format(parser, formatter))
      case Failure(error)             => fail(error)
      case Success(_)                 => assert(true)
    }
  }

  def checkError(snippet: String, expectedError: String): Unit = {
    val parser = new ScalaParser(snippet.stripMargin)
    parser.CompilationUnit.run() match {
      case Failure(e: ParseError) => assert(e.format(parser, formatter) == expectedError.stripMargin)
      case Failure(e)             => fail(e)
      case Success(_)             => fail("Parsing unexpectedly succeeded")
    }
  }

}
