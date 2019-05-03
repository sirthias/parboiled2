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

package org.parboiled.examples

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import org.parboiled2.examples.JsonParser

@State(Scope.Thread)
class TestState {
  // 744kb test JSON produced with http://www.json-generator.com/
  val json = io.Source.fromInputStream(getClass.getResourceAsStream("/test.json")).mkString
}

class JsonParserBenchmark {

  @Benchmark
  def sprayJsonParser(state: TestState) =
    spray.json.JsonParser(state.json)

  @Benchmark
  def parboiled2JsonParser(state: TestState) =
    new JsonParser(state.json).Json.run().get

  @Benchmark
  def json4SNative(state: TestState) =
    org.json4s.native.JsonMethods.parse(state.json)

  @Benchmark
  def json4SJackson(state: TestState) =
    org.json4s.jackson.JsonMethods.parse(state.json)
}
