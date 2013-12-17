import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

val commonSettings = Seq(
  version := "2.0.0-SNAPSHOT",
  scalaVersion := "2.10.3",
  organization := "parboiled.org",
  homepage := Some(new URL("http://parboiled.org")),
  description := "Fast and elegant PEG parsing in Scala - lightweight, easy-to-use, powerful",
  startYear := Some(2009),
  licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scalacOptions ++= List(
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-language:_",
    "-target:jvm-1.6",
    "-Xlog-reflective-calls"
  ),
  resolvers ++= Seq(Resolver.sonatypeRepo("snapshots"), Resolver.sonatypeRepo("releases")),
  shellPrompt := { s => Project.extract(s).currentProject.id + " > " })

val formattingSettings = scalariformSettings ++ Seq(
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(RewriteArrowSymbols, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(PreserveDanglingCloseParenthesis, true))

val publishingSettings = Seq(
  publishMavenStyle := true,
  useGpg := true,
  publishTo <<= version { v: String =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
    else                             Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomIncludeRepository := { _ => false },
  pomExtra :=
    <scm>
      <url>git@github.com:sirthias/parboiled2.git</url>
      <connection>scm:git:git@github.com:sirthias/parboiled2.git</connection>
    </scm>
    <developers>
      <developer>
        <id>sirthias</id>
        <name>Mathias Doenitz</name>
      </developer>
      <developer>
        <id>alexander-myltsev</id>
        <name>Alexander Myltsev</name>
        <url>http://www.linkedin.com/in/alexandermyltsev</url>
      </developer>
    </developers>
)

/////////////////////// DEPENDENCIES /////////////////////////

val scalaReflect = "org.scala-lang"  %  "scala-reflect"    % "2.10.3"   % "compile"
val shapeless    = "com.chuusai"     %  "shapeless_2.10.3" % "2.0.0-M1" % "compile"
val specs2       = "org.specs2"      %% "specs2-core"      % "2.3.6"    % "test"

/////////////////////// PROJECTS /////////////////////////

lazy val parboiled = project
  .settings(commonSettings: _*)
  .settings(formattingSettings: _*)
  .settings(publishingSettings: _*)
  .settings(
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full),
    libraryDependencies ++= Seq(scalaReflect, shapeless, specs2)
  )

lazy val examples = project
  .dependsOn(parboiled)
  .settings(commonSettings: _*)
  .settings(cappiSettings: _*)
  .settings(libraryDependencies ++= Seq(
    specs2,
    "io.spray" %%  "spray-json" % "1.2.5",
    "org.json4s" %% "json4s-native" % "3.2.6",
    "org.json4s" %% "json4s-jackson" % "3.2.6"
  ))