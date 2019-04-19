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
 * Generate action ops boilerplate for consuming/producing up to 22 values from/to the value stack.
 *
 * @author Mathias Doenitz
 * @author Alexander Myltsev
 */
object ActionOpsBoilerplate {

  private val MaxArity = 22

  def apply(srcManagedDir: File, streams: Keys.TaskStreams): Seq[File] = {
    val actionOpsManagedSourceFile = srcManagedDir / "ActionOps.scala"
    if (!actionOpsManagedSourceFile.exists()) {
      streams.log.info("Generating boilerplate ActionOps source file " + actionOpsManagedSourceFile)
      val scalaSource = (1 to MaxArity).map(generate).mkString(header + generate0, "\n", "}")
      IO.write(actionOpsManagedSourceFile, scalaSource)
    }
    Seq(actionOpsManagedSourceFile)
  }

  def header =
    """/*
      | * Copyright (C) 2009-2014 Mathias Doenitz, Alexander Myltsev
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
      |
      |package org.parboiled2.support
      |
      |import shapeless._
      |import org.parboiled2.Rule
      |
      |// provides the supported `~>` "overloads" for rules of type `Rule[I, O]` as `Out`
      |// as a phantom type, which is only used for rule DSL typing
      |
      |sealed trait ActionOps[I <: HList, O <: HList] { type Out }
      |object ActionOps {
      |  private type SJoin[I <: HList, O <: HList, R] = Join[I, HNil, O, R]
      |
      |""".stripMargin

  val AtoZ = 'A' to 'Z'
  val `A, ...` = Array.tabulate(MaxArity + 1)(AtoZ take _ mkString ", ")
  val `A ::...` = Array.tabulate(MaxArity + 1)(AtoZ take _ mkString " :: ")
  val `..., Z` = Array.tabulate(MaxArity + 1)(AtoZ takeRight _ mkString ", ")
  val `... :: Z` = Array.tabulate(MaxArity + 1)(AtoZ takeRight _ mkString " :: ")

  def generate0 = {
    def consumeStack(i: Int) =
      s"""    def apply[${`..., Z`(i)}, RR](f: (${`..., Z`(i)}) => RR)
         |        (implicit j: SJoin[${`... :: Z`(i)} :: II, HNil, RR],
         |                  c: FCapture[(${`..., Z`(i)}) => RR]): Rule[j.In, j.Out]
       """.stripMargin

    // implicit def ops0[II <: HList, OO <: HNil]: ActionOps[II, OO] { type Out = Ops0[II] } = `n/a`
    // sealed trait Ops0[II <: HList] {
    //   def apply[RR](f: () => RR)(implicit j: SJoin[II, HNil, RR], c: FCapture[() => RR]): Rule[j.In, j.Out]
    //   def apply[Z, RR](f: Z => RR)(implicit j: SJoin[Z :: II, HNil, RR], c: FCapture[Z => RR]): Rule[j.In, j.Out]
    //   def apply[Y, Z, RR](f: (Y, Z) => RR)(implicit j: SJoin[Y :: Z :: II, HNil, RR], c: FCapture[(Y, Z) => RR]): Rule[j.In, j.Out]
    //   def apply[X, Y, Z, RR](f: (X, Y, Z) => RR)(implicit j: SJoin[X :: Y :: Z :: II, HNil, RR], c: FCapture[(X, Y, Z) => RR]): Rule[j.In, j.Out]
    //   ...
    // }

    s"""  implicit def ops0[II <: HList, OO <: HNil]: ActionOps[II, OO] { type Out = Ops0[II] } = `n/a`
       |  sealed trait Ops0[II <: HList] {
       |    def apply[RR](f: () => RR)(implicit j: SJoin[II, HNil, RR], c: FCapture[() => RR]): Rule[j.In, j.Out]
       |
       |${(1 to MaxArity) map consumeStack mkString "\n"}
       |  }
    """.stripMargin
  }

  def generate(i: Int) = {
    def consumeOut(j: Int) = {
      val consumedTypes = AtoZ take i takeRight j mkString ", "
      s"""    def apply[RR](f: ($consumedTypes) => RR)
         |        (implicit j: SJoin[II, ${`A ::...`(i - j)} :: HNil, RR],
         |                  c: FCapture[($consumedTypes) => RR]): Rule[j.In, j.Out]
       """.stripMargin
    }

    def consumeStack(j: Int) =
      s"""    def apply[${`..., Z`(j)}, RR](f: (${`..., Z`(j)}, ${`A, ...`(i)}) => RR)
         |        (implicit j: SJoin[${`... :: Z`(j)} :: II, HNil, RR],
         |                  c: FCapture[(${`..., Z`(j)}, ${`A, ...`(i)}) => RR]): Rule[j.In, j.Out]
       """.stripMargin

    // e.g.
    // implicit def ops3[II <: HList, A, B, C]: ActionOps[II, A :: B :: C :: HNil] { type Out = Ops3[II, A, B, C] } = `n/a`
    // sealed trait Ops3[II <: HList, A, B, C] {
    //   def apply[RR](f: () => RR)(implicit j: SJoin[II, A :: B :: C :: HNil, RR], c: FCapture[() => RR]): Rule[j.In, j.Out]
    //   def apply[RR](f: C => RR)(implicit j: SJoin[II, A :: B :: HNil, RR], c: FCapture[C => RR]): Rule[j.In, j.Out]
    //   def apply[RR](f: (B, C) => RR)(implicit j: SJoin[II, A :: HNil, RR], c: FCapture[(B, C) => RR]): Rule[j.In, j.Out]
    //   def apply[RR](f: (A, B, C) => RR)(implicit j: SJoin[II, HNil, RR], c: FCapture[(A, B, C) => RR]): Rule[j.In, j.Out]
    //   def apply[Z, RR](f: (Z, A, B, C) => RR)(implicit j: SJoin[Z :: II, HNil, RR], c: FCapture[(Z, A, B, C) => RR]): Rule[j.In, j.Out]
    //   def apply[Y, Z, RR](f: (Y, Z, A, B, C) => RR)(implicit j: SJoin[Y :: Z :: II, HNil, RR], c: FCapture[(Y, Z, A, B, C) => RR]): Rule[j.In, j.Out]
    //   def apply[X, Y, Z, RR](f: (X, Y, Z, A, B, C) => RR)(implicit j: SJoin[X :: Y :: Z :: II, HNil, RR], c: FCapture[(X, Y, Z, A, B, C) => RR]): Rule[j.In, j.Out]
    //   ...
    // }

    s"""
       |  implicit def ops$i[II <: HList, ${`A, ...`(i)}]: ActionOps[II, ${`A ::...`(i)} :: HNil] { type Out = Ops$i[II, ${`A, ...`(i)}] } = `n/a`
       |  sealed trait Ops$i[II <: HList, ${`A, ...`(i)}] {
       |    def apply[RR](f: () => RR)(implicit j: SJoin[II, ${`A ::...`(i)} :: HNil, RR], c: FCapture[() => RR]): Rule[j.In, j.Out]
       |
       |${(1 to i-1) map consumeOut mkString "\n"}
       |    def apply[RR](f: (${`A, ...`(i)}) => RR)(implicit j: SJoin[II, HNil, RR], c: FCapture[(${`A, ...`(i)}) => RR]): Rule[j.In, j.Out]
       |
       |${(1 to 22-i) map consumeStack mkString "\n"}
       |  }
     """.stripMargin
  }
}
