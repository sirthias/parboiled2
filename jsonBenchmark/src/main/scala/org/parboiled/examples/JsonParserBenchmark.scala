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
    JsonParser.Json.runWithContext(state.json, new JsonParser.Context).get

  @Benchmark
  def json4SNative(state: TestState) =
    org.json4s.native.JsonMethods.parse(state.json)

  @Benchmark
  def json4SJackson(state: TestState) =
    org.json4s.jackson.JsonMethods.parse(state.json)

  @Benchmark
  def argonaut_(state: TestState) =
    argonaut.Parse.parseOption(state.json).get

  @Benchmark
  def parserCombinators(state: TestState): Unit =
    util.parsing.json.JSON.parseRaw(state.json).get
}