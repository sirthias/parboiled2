/*
 * Copyright (c) 2011-14 Miles Sabin, Mathias Doenitz, Alexander Myltsev 
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

import sbt._

/**
 * Generate a range of boilerplate classes, those offering alternatives with 0-22 params
 * and would be tedious to craft by hand
 *
 * @author Miles Sabin
 * @author Kevin Wright
 * @author Mathias Doenitz
 * @author Alexander Myltsev
 */
object Boilerplate {

  import scala.StringContext._

  implicit class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map {
        _ dropWhile (_.isWhitespace)
      }
      trimmedLines mkString "\n"
    }
  }


  def gen(dir: File) = {
    val actionOps = dir / "parboiled-core" / "ActionOps1.scala"
    IO.write(actionOps, GenActionOpsInstances.body)

    Seq(
      actionOps
    )
  }

  val header = """
                 |/*
                 | * Copyright (C) 2009-2013 Mathias Doenitz, Alexander Myltsev
                 | *
                 | * Licensed under the Apache License, Version 2.0 (the "License");
                 | * you may not use this file except in compliance with the License.
                 | * You may obtain a copy of the License at
                 | *
                 | * http://www.apache.org/licenses/LICENSE-2.0
                 | *
                 | * Unless required by applicable law or agreed to in writing, software
                 | * distributed under the License is distributed on an "AS IS" BASIS,
                 | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
                 | * See the License for the specific language governing permissions and
                 | * limitations under the License.
                 | */
               """.stripMargin

  class TemplateVals(val arity: Int) {
    val synTypes     = (0 until arity) map (n => s"A$n")
    val synVals      = (0 until arity) map (n => s"a$n")
    val synTypedVals = (synVals zip synTypes) map { case (v, t) => v + ":" + t}

    val `A..N`       = synTypes.mkString(", ")
    val `A..N,Res`   = (synTypes :+ "Res") mkString ", "
    val `a..n`       = synVals.mkString(", ")
    val `A::N`       = synTypes mkString "::"
    val `A::N::HNil` = (synTypes :+ "HNil") mkString " :: "
    val `a::n`       = (synVals :+ "HNil") mkString " :: "
    val `(A..N)`     = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)`     = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)`     = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N`   = synTypedVals mkString ", "
  }

  trait Template {
    def content(tv: TemplateVals): String

    def range = 1 to 22

    def body: String = {
      val headerLines = header split '\n'
      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty)}
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {
        _ filter (_ startsWith "-") map (_.tail)
      }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (headerLines ++ preBody ++ instances ++ postBody) mkString "\n"
    }
  }

  object GenActionOpsInstances extends Template { t => 
    override def range = 0 to 22

    def content(tv: TemplateVals) = {
      import tv._

      def synTypesRev(arity: Int) = (0 until arity).map { n => s"Z$n" }.reverse

      def `O|A::N::HNil`(arity: Int) =
        if (arity == 0) "O"
        else s"${`A::N::HNil`}"
      
      def `O|A..N`(arity: Int) =
        if (arity == 0) "O <: HNil"
        else s"${`A..N`}"

      def `|A..N`(arity: Int) =
        if (arity == 0) ""
        else s"${`A..N`}"

      def `S..Z,A..N`(arity: Int) =
        (synTypesRev(arity) ++ synTypes).takeRight(arity).mkString(", ")

      def `S..Z,R`(arity: Int) =
        (synTypesRev(arity) :+ 'R').mkString(", ")

      def `S::Z::I`(arity: Int) =
        (synTypesRev(arity) :+ 'I').mkString(" :: ")

      def `A::N::HNil_var`(arity: Int) =
        (synTypes.take(math.max(arity, 0)) :+ "HNil").mkString(" :: ")

      def comma(arity: Int) =
        if (arity == 0) "" else ", "

      block"""
        |package org.parboiled2.support
        |
        |import org.parboiled2.Rule
        |import shapeless._
        |import shapeless.ops.hlist.ReversePrepend
        |
        |sealed trait ActionOps[I <: HList, O <: HList] { type Out }
        |object ActionOps {
        |  private type SJoin[I <: HList, O <: HList, R] = Join[I, HNil, O, R]
        |  
        -  implicit def ops${arity}[I <: HList, ${`O|A..N`(arity)}]: ActionOps[I, ${`O|A::N::HNil`(arity)}] { 
        -    type Out = Ops${arity}[I${comma(arity)}${`|A..N`(arity)}] } = `n/a`
        -
        -  sealed trait Ops${arity}[I <: HList${comma(arity)}${`|A..N`(arity)}] {
          ${ range.map { i => s"""
       |-    def apply[${`S..Z,R`(i - arity)}](f: (${`S..Z,A..N`(i)}) => R)
       |-        (implicit j: SJoin[${`S::Z::I`(i - arity)}, ${`A::N::HNil_var`(arity - i)}, R],
       |-                  s: FCapture[(${`S..Z,A..N`(i)}) => R]): Rule[j.In, j.Out]
       |-
       |""".stripMargin
           } mkString "\n" }
        -  }
        -
        |
        |}
      """
    }
  }
}
