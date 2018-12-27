import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._
import scala.xml.transform._
import scala.xml.{Node => XNode, NodeSeq}
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.CrossPlugin.autoImport._

val commonSettings = Seq(
  version := "2.1.5",
  scalaVersion := "2.12.7",
  crossScalaVersions := Seq("2.11.12", "2.12.7", "2.13.0-M5"),
  organization := "org.parboiled",
  homepage := Some(new URL("http://parboiled.org")),
  description := "Fast and elegant PEG parsing in Scala - lightweight, easy-to-use, powerful",
  startYear := Some(2009),
  licenses := Seq("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-source", "1.6",
    "-target", "1.6",
    "-Xlint:unchecked",
    "-Xlint:deprecation"),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 11 => Seq("-target:jvm-1.6")
      case _ => Nil
    }
  },
  scalacOptions ++= List(
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-language:_",
    "-Xlog-reflective-calls"))

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
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
    else                                         Some("releases" at nexus + "service/local/staging/deploy/maven2")
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

def scalaReflect(v: String) = "org.scala-lang"  %  "scala-reflect"     % v       % "provided"
val shapeless               = Def.setting("com.chuusai"     %%% "shapeless"         % "2.3.3" % "compile")
val specs2Core              = Def.setting("org.specs2"      %%% "specs2-core"       % "4.3.6" % "test")
val specs2ScalaCheck        = Def.setting("org.specs2"      %%% "specs2-scalacheck" % "4.3.6" % "test")

/////////////////////// PROJECTS /////////////////////////

lazy val root = project.in(file("."))
  .aggregate(examples, jsonBenchmark)
  .aggregate(parboiledJVM, parboiledJS)
  .aggregate(parboiledCoreJVM, parboiledCoreJS)
  .settings(commonSettings)
  .settings(noPublishingSettings)

lazy val examples = project
  .dependsOn(parboiledJVM)
  .settings(commonSettings)
  .settings(noPublishingSettings)
  .settings(libraryDependencies ++= Seq(specs2Core.value, "io.spray" %%  "spray-json" % "1.3.5"))

lazy val bench = inputKey[Unit]("Runs the JSON parser benchmark with a simple standard config")

lazy val jsonBenchmark = project
  .dependsOn(examples)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(noPublishingSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-native" % "3.6.3",
      "org.json4s" %% "json4s-jackson" % "3.6.3"),
    bench := (run in Compile).partialInput(" -i 10 -wi 10 -f1 -t1").evaluated)

lazy val scalaParser = project
  .dependsOn(parboiledJVM)
  .settings(commonSettings)
  .settings(noPublishingSettings)
  .settings(libraryDependencies ++= Seq(shapeless.value, specs2Core.value))

lazy val parboiledOsgiSettings = osgiSettings ++ Seq(
  OsgiKeys.exportPackage := Seq("org.parboiled2.*;version=${Bundle-Version}"),
  OsgiKeys.privatePackage := Seq()
)

lazy val parboiledJVM = parboiled.jvm
lazy val parboiledJS = parboiled.js
lazy val parboiled = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(parboiledCore)
  .settings(commonSettings)
  .settings(formattingSettings)
  .settings(publishingSettings)
  .jvmSettings(
    mappings in (Compile, packageBin) ++= (mappings in (parboiledCoreJVM.project, Compile, packageBin)).value,
    mappings in (Compile, packageSrc) ++= (mappings in (parboiledCoreJVM.project, Compile, packageSrc)).value,
    mappings in (Compile, packageDoc) ++= (mappings in (parboiledCoreJVM.project, Compile, packageDoc)).value
  )
  .jsSettings(
    mappings in (Compile, packageBin) ++= (mappings in (parboiledCoreJS.project, Compile, packageBin)).value,
    mappings in (Compile, packageSrc) ++= (mappings in (parboiledCoreJS.project, Compile, packageSrc)).value,
    mappings in (Compile, packageDoc) ++= (mappings in (parboiledCoreJS.project, Compile, packageDoc)).value
  )
  .settings(
    libraryDependencies ++= Seq(scalaReflect(scalaVersion.value), shapeless.value, specs2Core.value),
    mappings in (Compile, packageBin) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    mappings in (Compile, packageDoc) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    pomPostProcess := { // we need to remove the dependency onto the parboiledCore module from the POM
      val filter = new RewriteRule {
        override def transform(n: XNode) = if ((n \ "artifactId").text.startsWith("parboiledcore")) NodeSeq.Empty else n
      }
      new RuleTransformer(filter).transform(_).head
    }
  ).
  enablePlugins(SbtOsgi).settings(parboiledOsgiSettings:_*)

lazy val generateActionOps = taskKey[Seq[File]]("Generates the ActionOps boilerplate source file")

lazy val parboiledCore = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).in(file("parboiled-core"))
  .settings(commonSettings)
  .settings(formattingSettings)
  .settings(noPublishingSettings)
  .settings(
    libraryDependencies ++= Seq(scalaReflect(scalaVersion.value), shapeless.value, specs2Core.value, specs2ScalaCheck.value),
    generateActionOps := ActionOpsBoilerplate((sourceManaged in Compile).value, streams.value),
    (sourceGenerators in Compile) += generateActionOps.taskValue)

lazy val parboiledCoreJVM = parboiledCore.jvm
lazy val parboiledCoreJS = parboiledCore.js
