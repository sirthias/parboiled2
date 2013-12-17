package org.parboiled2.examples

class JsonParserBenchmark extends com.google.caliper.SimpleBenchmark {

  // 744kb test JSON produced with http://www.json-generator.com/
  val jsonSource = io.Source.fromInputStream(getClass.getResourceAsStream("/test.json")).mkString

  def timeParboiled1JsonParser(reps: Int): Unit =
    for (i <- 0 to reps)
      spray.json.JsonParser(jsonSource) // the spray-json parser is implemented with parboiled 1.x

  def timeParboiled2JsonParser(reps: Int): Unit =
    for (i <- 0 to reps)
      new JsonParser(jsonSource).Json.run().get

  def timeJson4SNative(reps: Int): Unit =
    for (i <- 0 to reps)
      org.json4s.native.JsonMethods.parse(jsonSource)

  def timeJson4SJackson(reps: Int): Unit =
    for (i <- 0 to reps)
      org.json4s.jackson.JsonMethods.parse(jsonSource)
}