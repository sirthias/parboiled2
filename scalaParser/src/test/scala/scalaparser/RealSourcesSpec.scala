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

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import utest.*
import org.parboiled2.*

object RealSourcesSpec extends TestSuite {

  val tests = Tests {

    "The ScalaParser should successfully parse the following project sources" - {

      "parboiled2" - checkDir(".")

      "akka" - checkDir("~/Documents/projects/Akka")

      "shapeless" - checkDir("~/Documents/forks/shapeless")

      "spray" - checkDir("~/Documents/projects/spray")

      "scalaz" - checkDir("~/Documents/forks/scalaz")

      "spire" - checkDir("~/Documents/forks/spire")

      "sbt" - checkDir(
        "~/Documents/forks/xsbt",
        "sbt/std/InputWrapper.scala", // unicode escapes
        "sbt/src/sbt-test",
        "util/cross/src/main/input_sources"
      )

      "scala" - checkDir(
        "~/Documents/forks/scala",
        // Not real Scala files
        "dbuild-meta-json-gen.scala",
        "genprod.scala",
        "disabled",                        // don't bother parsing disabled tests
        "neg",                             // or neg tests
        "deprecate-early-type-defs.scala", // or deprecated tests
        // or unicode escapes
        "test/files/run/literals.scala",
        "test/files/run/t3835.scala",
        "test/files/run/richs.scala",
        // Lots of guys in these folders seem to be borked, skip all of them
        "test/files/positions",
        "test/files/presentation",
        "test/pending"
      )
    }
  }

  val utf8      = Charset.forName("UTF-8")
  val formatter = new ErrorFormatter(showTraces = true)

  def checkDir(path: String, blackList: String*): String => Boolean = { exampleName =>
    def checkFile(path: String): Int = {
      val inputStream = new FileInputStream(path)
      val utf8Bytes   = Array.ofDim[Byte](inputStream.available)
      inputStream.read(utf8Bytes)
      inputStream.close()
      val charBuffer        = utf8.decode(ByteBuffer.wrap(utf8Bytes))
      val parser            = new ScalaParser(ParserInput(charBuffer.array(), charBuffer.remaining()))
      def fail(msg: String) = Predef.assert(false, msg)
      parser.CompilationUnit.run().failed foreach {
        case error: ParseError => fail(s"Error in file `$path`:\n" + error.format(parser, formatter))
        case error             => fail(s"Exception in file `$path`:\n$error")
      }
      parser.input.length
    }
    def listFiles(file: File): Iterator[String] = {
      val (dirs, files) = file.listFiles().iterator.partition(_.isDirectory)
      files.map(_.getPath) ++ dirs.flatMap(listFiles)
    }

    val startTime = System.nanoTime()
    val fileChars =
      for {
        fileName <- listFiles(new File(if (path startsWith "~") System.getProperty("user.home") + path.tail else path))
        if fileName endsWith ".scala"
        if !blackList.exists(fileName.contains)
      } yield checkFile(fileName)
    val totalChars = fileChars.sum / 1000
    val millis     = (System.nanoTime() - startTime) / 1000000
    println(s"$exampleName:\n  ${totalChars}K chars in $millis ms (${totalChars * 1000 / millis}K chars/sec})")
    true
  }
}
