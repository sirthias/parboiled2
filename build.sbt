import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._
import scala.xml.transform._
import scala.xml.{Node => XNode, NodeSeq}

val commonSettings = Seq(
  version := "2.0.0",
  scalaVersion := "2.11.1",
  organization := "org.parboiled",
  homepage := Some(new URL("http://parboiled.org")),
  description := "Fast and elegant PEG parsing in Scala - lightweight, easy-to-use, powerful",
  startYear := Some(2009),
  licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-source", "1.6",
    "-target", "1.6",
    "-Xlint:unchecked",
    "-Xlint:deprecation"),
  scalacOptions ++= List(
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-language:_",
    "-target:jvm-1.6",
    "-Xlog-reflective-calls"),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    "spray repo" at "http://repo.spray.io"))

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
    </developers>)

val noPublishingSettings = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo"))))

/////////////////////// DEPENDENCIES /////////////////////////

val scalaReflect     = "org.scala-lang"  %  "scala-reflect"     % "2.11.1"   % "provided"
val shapeless        = "com.chuusai"     %% "shapeless"         % "2.0.0"    % "compile"
val specs2Core       = "org.specs2"      %% "specs2-core"       % "2.3.11"   % "test"
val specs2ScalaCheck = "org.specs2"      %% "specs2-scalacheck" % "2.3.11"   % "test"

/////////////////////// PROJECTS /////////////////////////

lazy val root = project.in(file("."))
  .aggregate(examples, parboiled, parboiledCore)
  .settings(noPublishingSettings: _*)

lazy val examples = project
  .dependsOn(parboiled)
  .settings(commonSettings: _*)
  .settings(cappiSettings: _*)
  .settings(noPublishingSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      specs2Core,
      "io.spray" %%  "spray-json" % "1.2.6",
      "org.json4s" %% "json4s-native" % "3.2.9",
      "org.json4s" %% "json4s-jackson" % "3.2.9",
      "io.argonaut" %% "argonaut" % "6.0.4"))

lazy val parboiled = project
  .dependsOn(parboiledCore)
  .settings(commonSettings: _*)
  .settings(formattingSettings: _*)
  .settings(publishingSettings: _*)
  .settings(
    libraryDependencies ++= Seq(scalaReflect, shapeless, specs2Core),
    mappings in (Compile, packageBin) ++= (mappings in (parboiledCore.project, Compile, packageBin)).value,
    mappings in (Compile, packageSrc) ++= (mappings in (parboiledCore.project, Compile, packageSrc)).value,
    mappings in (Compile, packageDoc) ++= (mappings in (parboiledCore.project, Compile, packageDoc)).value,
    mappings in (Compile, packageBin) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    mappings in (Compile, packageDoc) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    pomPostProcess := { // we need to remove the dependency onto the parboiledCore module from the POM
      val filter = new RewriteRule {
        override def transform(n: XNode) = if ((n \ "artifactId").text.startsWith("parboiledcore")) NodeSeq.Empty else n
      }
      new RuleTransformer(filter).transform(_).head
    }
  )

lazy val parboiledCore = project.in(file("parboiled-core"))
  .settings(commonSettings: _*)
  .settings(formattingSettings: _*)
  .settings(noPublishingSettings: _*)
  .settings(
    libraryDependencies ++= Seq(scalaReflect, shapeless, specs2Core, specs2ScalaCheck))