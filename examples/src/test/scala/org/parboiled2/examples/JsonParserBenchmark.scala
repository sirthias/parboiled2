package org.parboiled2.examples

class JsonParserBenchmark extends com.google.caliper.SimpleBenchmark {
  def timeParboiled1JsonParser(reps: Int): Unit =
    for (i <- 0 to reps)
      spray.json.JsonParser(Test.json) // the spray-json parser is implemented with parboiled 1.x

  def timeParboiled2JsonParser(reps: Int): Unit =
    for (i <- 0 to reps)
      new JsonParser(Test.json).Json.run().get

  def timeJson4SNative(reps: Int): Unit =
    for (i <- 0 to reps)
      org.json4s.native.JsonMethods.parse(Test.json)

  def timeJson4SJackson(reps: Int): Unit =
    for (i <- 0 to reps)
      org.json4s.jackson.JsonMethods.parse(Test.json)

  def timeArgonaut(reps: Int): Unit =
    for (i <- 0 to reps)
      argonaut.Parse.parseOption(Test.json).get

  def timeParserCombinators(reps: Int): Unit =
    for (i <- 0 to reps)
      util.parsing.json.JSON.parseRaw(Test.json).get
}