package scalaparser

import scala.util.{Success, Failure}
import org.specs2.execute.FailureException
import org.specs2.mutable.Specification
import org.specs2.specification.Fragment
import org.parboiled2._

class SnippetSpec extends Specification {

  "The ScalaParser should properly parse the positive example snippets" >> {

    check("001", "package torimatomeru")

    check("002",
      """
        |package torimatomeru
        |
        |package lols""")
    
    check("003",
      """package torimatomeru
        |import a
        |import b
      """)
    
    check("004",
      """package torimatomeru
        |
        |import org.parboiled2.ParseError
        |import utest._
        |import utest.framework.Test
        |import utest.util.Tree
        |
        |import scala.util.{Failure, Success}
        |
        |object SyntaxTest extends TestSuite""")
    
    check("005",
      """object SyntaxTest extends TestSuite{
        |  def check[T](input: String) = {
        |
        |  }
        |}""")
    
    check("006",
      """object SyntaxTest{
        |  a()
        |  throw 1
        |}""")
    
    check("007",
      """object SyntaxTest extends TestSuite{
        |  {
        |        println
        |        throw 1
        |  }
        |}""")
    
    check("008",
      """package scalatex
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
    
    check("009",
      """object Moo{
        |  a
        |  .b
        |
        |  c
        |}""")
    
    check("010",
      """object Moo{
        | filename
        |        .asInstanceOf[Literal]
        |10
        |}""")

    check("011",
      """object Cow{
        |  ().mkString
        |
        |  1
        |}""")
    
    check("012",
      """object O{
        | private[this] val applyMacroFull = 1
        |}""")
    
    check("013",
      """object O{
        | private[this] def applyMacroFull(c: Context)
        |                      (expr: c.Expr[String],
        |                       runtimeErrors: Boolean,
        |                       debug: Boolean)
        |                      : c.Expr[Frag] = {
        |                      }
        |}""")
    
    check("014",
      """object O{
        |  class DebugFailure extends Exception
        |
        |  1
        |}""")
    
    check("015",
      """package torimatomeru
        |
        |package syntax
        |
        |import org.parboiled2._
        |
        |""")
    
    check("016",
      """object Foo{
        |  0 match {
        |    case A | B => 0
        |  }
        |}""")
    
    check("017",
      """object Compiler{
        |
        |  def apply = {
        |    def rec = t match {
        |      case 0 => 0
        |    }
        |
        |    rec(tree)
        |  }
        |}""")
    
    check("018",
      """object O {
        |    A(A(A(A(A(A(A(A())))))))
        |}""")
    
    check("019",
      """object O{
        |   A(A(A(A(A(A(A(A(A(A(A(A(A(A(A(A())))))))))))))))
        |}""")
    
    check("020",
      """object L{
        |  a.b = c
        |  a().b = c
        |}      """)
    
    check("021",
      """object L{
        |  a b c
        |  d = 1
        |}""")

    check("022",
      """/*                     __                                               *\
        |**     ________ ___   / /  ___      __ ____  Scala.js CLI               **
        |**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
        |**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
        |** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
        |**                          |/____/                                     **
        |\*                                                                      */
        |
        |package scala.scalajs.cli
        |""")
    
    check("023",
      """object O{
        |  for {
        |      a  <- b
        |      c <- d
        |  } {
        |    1
        |  }
        |}""")
    
    check("024",
      """object O{
        |  val jarFile =
        |      try { 1 }
        |      catch { case _: F => G }
        |}""")
    
    check("025",
      """object F{
        |  func{ case _: F => fail }
        |}""")
    
    check("026",
      """object Foo{
        |    val a = d // g
        |    val b = e // h
        |    val c = f
        |}""")
    
    check("027",
      """object L{
        |  x match{
        |    case y.Y(z) => z
        |  }
        |}""")
    
    check("028",
      """object K{
        |  val a: B {
        |    val c: D
        |  }
        |
        |  1
        |}""")
    
    check("029",
      """object LOLS{
        |    def run() {}
        |
        |    def apply() {}
        |}""")
    
    check("030",
      """object O{
        |  a =:= b.c
        |}""")
    
    check("031",
      """object K{
        |  a(
        |    1: _*
        |  )
        |}""")
    
    check("032",
      """object P{
        |      tree match {
        |        case stats :+ expr  => 1
        |      }
        |}""")
    
    check("033",
      """object K{
        |  val trueA = 1
        |}""")
    
    check("034",
      """object K{
        |  val nullo :: cow = 1
        |}""")
    
    check("035",
      """object K{
        |  val omg_+ = 1
        |}""")
    
    check("036",
      """object K{
        |  val + = 1
        |  var * = 2
        |}""")
    
    check("037",
      """object O{
        |  c match {
        |    case b_  => 1
        |  }
        |}""")
    
    check("038",
      """trait Basic {
        |  b match {
        |    case C => true; case _ => false
        |  }
        |}""")
    
    check("039",
      """trait Basic {
        |  !a.b
        |}""")
    
    check("040",
      """class Parser {
        |  {() => }
        |}
        |""")
    
    check("041",
      """
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
    
    check("042",
      """object GenJSCode {
        |  code: @switch
        |}""")
    
    check("043",
      """object B {
        |  { a: L => }
        |}""")
    
    check("044",
      """object O{
        |  {
        |    val index = 0
        |    i: Int => 10
        |    0
        |  }
        |}""")
    
    check("045",
      """object GenJSCode{
        |  val g: G.this.g.type
        |}
        |""")
    
    check("046",
      """object K{
        |  class RTTypeTest
        |  private object O
        |}""")
    
    check("047",
      """object O{
        |  if (eqeq &&
        |
        |    false)  1
        |}
      """)
    
    check("048",
      """object O{
        |  for(
        |    x <- Nil map
        |
        |  (x => x)
        |  ) yield x
        |}""")
    
    check("049",
      """object O{
        |  for{
        |    x <- Nil
        |    if
        |
        |    1 == 2
        |  } yield x
        |}""")
    
    check("050",
      """object ScopedVar {
        |  def withScopedVars(ass: Seq[_]) = 1
        |}""")
    
    check("051",
      """abstract class JSASTTest extends DirectTest {
        |  def show: this.type = ()
        |}""")
    
    check("052",
      """object Traversers {
        |  {
        |        1
        |        cases foreach nil
        |  }
        |}""")
    
    check("053",
      """object Utils {
        |  "\\"
        |}
        |""")
    
    check("054",
      """object F{
        |  this eq that.asInstanceOf[AnyRef]
        |}""")
    
    check("055",
      """class C{
        |  0x00 <= 2 && 1
        |}
        |""")
    
    check("056",
      """class Runtime private
      """)
    
    check("057",
      """object System {
        |  def a[@b T[@b V]] = 1
        |}
        |""")
    
    check("058",
      """object U{
        |  private val _fragment = fld(Fragment)
        |  _fld = null
        |}""")
    
    check("059",
      """class Array{
        |  def length_= = 1
        |}""")
    
    check("060",
      """object K{
        |  def newBuilder =
        |    new B
        |
        |  @inline def a = 1
        |}""")
    
    check("061",
      """trait Function12[-T1, +R]
      """)
    
    check("062",
      """@a // Don't do this at home!
        |trait B
      """)
    
    check("063",
      """object T{
        |  type B = { def F: S }
        |}
        |""")
    
    check("064",
      """
        |object ScalaJSBuild{
        |      (
        |        1 / 2
        |          / 3
        |      )
        |}""")
    
    check("065",
      """trait Writer{
        | '\f'
        |}
      """)
    
    check("066",
      """object CyclicDependencyException {
        |    def str(info: ResolutionInfo) =
        |      s"${info.resourceName} from: ${info.origins.mkString(", ")}"
        |}""")
    
    check("067",
      """object OptimizerCore {
        |  tpe match {
        |    case NothingType | _:RecordType=> 1
        |  }
        |}""")
    
    check("068",
      """class A{
        |  1
        |  () => 1
        |}""")
    
    check("069",
      """trait ReactorCanReply {
        |  _: InternalReplyReactor =>
        |}""")

    check("070",
      """object G{
        |  def isBefore(pd: SubComponent) = settings.stopBefore
        |  phaseDescriptors sliding 2 collectFirst ()
        |}""")
    
    check("071",
      """class SymbolLoaders {
        |  type T = ClassPath[AbstractFile]#ClassRep
        |}""")
    
    check("072",
      """trait ContextErrors {
        |    def isUnaffiliatedExpr = expanded.isInstanceOf[scala.reflect.api.Exprs#Expr[_]]
        |}""")
    
    check("073",
      """trait Typers{
        |  s"nested ${ if (1) "trait" else "class" }"
        |}""")
    
    check("074",
      """trait ReflectSetup { this: Global =>
        |  phase = 1
        |}""")
    
    check("075",
      """trait Predef {
        |  @x
        |  // a
        |  type T
        |}""")
    
    check("076",
      """object StringContext {

            s"${
              require(index >= 0 && index < str.length)
              val ok = "[\b, \t, \n, \f, \r, \\, \", \']"
              if (index == str.length - 1) "at terminal" else s"'\\${str(index + 1)}' not one of $ok at"
            }"

          }
      """)
    
    check("077",
      """trait Growable {
        |    +=
        |}""")
    
    check("078",
      """package immutable {
        |  object O
        |}""")
    
    check("079",
      """import java.util.concurrent.TimeUnit.{ NANOSECONDS => NANOS, MILLISECONDS ⇒ MILLIS }
      """)
    
    check("080",
      """class FunFinder{
        |  val targetName = s"$name${ if (isModule) "$" else "" }"
        |}""")
    
    check("081",
      """class AkkaException{
        |  for (i ← 0 until trace.length)
        |    ()
        |}""")
    
    check("082",
      """object Test4 {
        |    type T = F @field
        |    @BeanProperty val x = 1
        |}""")
    
    check("083",
      """package `dmacro` {
        |}""")
    
    check("084",
      """class A {
        |  def fn1 = List apply 1
        |  def fn2 = List apply[Int] 2
        |}""")
    
    check("085",
      """class C {
        |  def this(x: Int) = {
        |    this();
        |    class D;
        |  }
        |}""")
    
    check("086",
      """trait B[T] {
        |  def f1(a: T): Unit { }
        |}""")
    
    check("087",
      """object test {
        |  case object Int16 extends SampleFormat1
        |  (1) match {
        |    case _   => 1
        |  }
        |}""")
    
    check("088",
      """object A {
        |  def x {
        |    implicit lazy val e: Int = 0
        |  }
        |}""")
    
    check("089",
      """object test {
        |  for {
        |    n <- A
        |    a <- B
        |    _ <- C
        |  } yield n
        |}""")
    
    //        check(
    //          """object Test {
    //            |  def t1: M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[M[Inty @unchecked]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] = x
    //            |}
    //          """
    //        }
    
    check("090",
      """abstract class Mix___eFoo___wBar_I_ extends Foo___ with Bar_I_    { ; ; f; }
      """)
    
    check("091",
      """package test2 {
        |object N1M0;
        |}""")
    
    check("092",
      """class IP extends {
        |  val baz = "bar";
        |} with Foo(() => baz);""")
    
    check("093",
      """object Test extends App {
        |  val x: C {} = 1
        |}""")
    
    check("094",
      """trait LensFunctions {
        |  type T = A @> B
        |}""")
    
    check("095",
      """object ContravariantCoyonedaUsage {
        |  (schwartzian[Vector[String], ccord.I]
        |      (unstructuredData)(v => ccord.k(v(i)))(ccord.fi))
        |}""")
    
    check("096",
      """object MapTest{
        |  forAll { a: Int ==>> Int =>
        |  }
        |}""")
    
    check("097",
      """object Test {
        |  def countingDownActor = {
        |    val ms = 1
        |    (m: Int) =>
        |      val x = 1
        |      1
        |  }
        |}""")
    
    check("098",
      """object X{
        |  type T = {
        |    ;;;
        |    type x = Int
        |    ;;;
        |    type y = Int
        |    ;;;
        |  }
        |}""")
    
    check("099",
      """object X{
        |  <div />
        |}""")
    
    check("100",
      """object X{
        |  <div id="hello" />
        |}""")
    
    check("101",
      """object X{
        |  <url>https://github.com/lihaoyi/scalatags</url>
        |}""")
    
    check("102",
      """object X{
        |  <url>{ ;;;1 + 1 }</url>
        |}""")
    
    check("103",
      """object UserAgentCalculator extends Factory {
        |    for {
        |      userAgent <- userAgent
        |      findResult = ieMatch.find if findResult
        |    } yield ver
        |}""")
    
    //        check(
    //          """class FunctionalBuilder{
    //            |  a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16), a17), a18), a19), a20), a21), a22)
    //            |}
    //          """
    //        }
    
    check("104",
      """class HtmlPage {
        |  <meta http-equiv="content-type" content={ 1 }/>
        |} """)
    
    check("105",
      """object K{
        |  <script> {{</script>
        |}""")
    
    check("106",
      """object O{
        |  e match { case <title>{ _* }</title> => }
        |}
        |""")
    
    check("107",
      """object Publish {
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
    
    check("108",
      """object K{
        |    <foo baz="&amp;dog"/>
        |}""")
    
    check("0109",
      """object X{
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

  "The ScalaParser should properly parse the negative example snippets" >> {
    checkError("500",
      "packge torimatomeru"
    ,
      """Invalid input 'g', expected 'a' (line 1, column 5):
        |packge torimatomeru
        |    ^
        |
        |3 rules mismatched at error location:
        |  /CompilationUnit/ /Body/ |:-4 /TopPackageSeq/ +:-4 /Key/ /RawKey/ "package":-4 / 'a'
        |  /CompilationUnit/ /Body/ |:-4 /TopStatSeq/ +:-4 /TopStat/ |:-4 /PkgBlock/ /Key/ /RawKey/ "package":-4 / 'a'
        |  /CompilationUnit/ /Body/ |:-4 /TopStatSeq/ +:-4 /TopStat/ |:-4 /PkgObj/ /Key/ /RawKey/ "package":-4 / 'a'
        |""".stripMargin)

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

  val formatter = new ErrorFormatter(showTraces = true)

  def check(nr: String, snippet: String): Fragment =
    s"example $nr" in {
      ScalaParser.CompilationUnit.run(snippet.stripMargin) match {
        case Failure(error: ParseError) => failure(error.format(snippet.stripMargin, formatter))
        case Failure(error) => failure(error.toString)
        case Success(_) => success
      }
    }

  def checkError(nr: String, snippet: String, expectedError: String): Fragment =
    s"example $nr" in {
      val error = ScalaParser.CompilationUnit.run(snippet.stripMargin) match{
        case Failure(e: ParseError) => e
        case Failure(e) => throw new FailureException(org.specs2.execute.Failure(e.toString))
        case Success(_) => throw new FailureException(org.specs2.execute.Failure("Parsing unexpectedly succeeded"))
      }
      error.format(snippet.stripMargin, formatter) === expectedError.stripMargin
    }
}
