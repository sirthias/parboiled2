libraryDependencies ++= Seq(
    "org.parboiled" % "parboiled-scala_2.10" % "1.1.5"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:_",
  "-encoding", "UTF-8"
)