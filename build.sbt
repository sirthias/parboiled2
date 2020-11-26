import ReleaseTransformations._
import sbtcrossproject.CrossPlugin.autoImport._

val commonSettings = Seq(
  organization := "org.parboiled",
  homepage := Some(new URL("http://parboiled.org")),
  description := "Fast and elegant PEG parsing in Scala - lightweight, easy-to-use, powerful",
  startYear := Some(2009),
  licenses := Seq("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  unmanagedResources in Compile += baseDirectory.value.getParentFile.getParentFile / "LICENSE",
  scmInfo := Some(
    ScmInfo(url("https://github.com/sirthias/parboiled2"), "scm:git:git@github.com:sirthias/parboiled2.git")
  ),
  scalaVersion := "2.12.12",
  crossScalaVersions := Seq("2.12.12", "2.13.3"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:_",
    "-unchecked",
    "-Xlint:_,-missing-interpolator",
    "-Ywarn-dead-code"
    //"-Ywarn-numeric-widen",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) =>
        Seq(
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
          "-Ycache-macro-class-loader:last-modified",
          "-Ybackend-parallelism",
          "8",
          "-Xfatal-warnings",
          "-Xfuture",
          "-Xsource:2.13" // new warning: deprecate assignments in argument position
        )
      case Some((2, 13)) =>
        Seq(
          "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
          "-Ycache-macro-class-loader:last-modified",
          "-Ybackend-parallelism",
          "8"
        )
      case x => sys.error(s"unsupported scala version: $x")
    }
  },
  scalacOptions in (Compile, console) ~= (_ filterNot (o ⇒ o == "-Ywarn-unused-import" || o == "-Xfatal-warnings")),
  scalacOptions in (Test, console) ~= (_ filterNot (o ⇒ o == "-Ywarn-unused-import" || o == "-Xfatal-warnings")),
  scalacOptions in (Compile, doc) += "-no-link-warnings",
  sourcesInBase := false,
  // file headers
  headerLicense := Some(HeaderLicense.ALv2("2009-2019", "Mathias Doenitz")),
  // reformat main and test sources on compile
  scalafmtOnCompile := true
)

lazy val crossSettings = Seq(
  sourceDirectories in (Compile, scalafmt) := (unmanagedSourceDirectories in Compile).value,
  sourceDirectories in (Test, scalafmt) := (unmanagedSourceDirectories in Test).value
)

lazy val scalajsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule).withSourceMap(false)),
  scalaJSStage in Global := FastOptStage,
  scalacOptions ~= { _.filterNot(_ == "-Ywarn-dead-code") }
)

lazy val publishingSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := (_ ⇒ false),
  publishTo := sonatypePublishTo.value,
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  developers := List(
    Developer("sirthias", "Mathias Doenitz", "devnull@bullet.io", url("https://github.com/sirthias")),
    Developer("alexander-myltsev", "Alexander Myltsev", "", url("http://www.linkedin.com/in/alexandermyltsev"))
  )
)

lazy val releaseSettings = {
  val runCompile = ReleaseStep(action = { st: State ⇒
    val extracted = Project.extract(st)
    val ref       = extracted.get(thisProjectRef)
    extracted.runAggregated(compile in Compile in ref, st)
  })

  Seq(
    releaseCrossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runCompile,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts,
      releaseStepCommand("sonatypeReleaseAll"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )
}

val utestSettings = Seq(testFrameworks := Seq(new TestFramework("utest.runner.Framework")))

lazy val parboiledOsgiSettings = osgiSettings ++ Seq(
  OsgiKeys.exportPackage := Seq("org.parboiled2.*;version=${Bundle-Version}"),
  OsgiKeys.privatePackage := Seq()
)

/////////////////////// DEPENDENCIES /////////////////////////

val shapeless       = Def.setting("com.chuusai" %%% "shapeless" % "2.3.3" % Compile)
val utest           = Def.setting("com.lihaoyi" %%% "utest" % "0.7.5" % Test)
val scalaCheck      = Def.setting("org.scalacheck" %%% "scalacheck" % "1.15.1" % Test)
val `scala-reflect` = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)

// benchmarks and examples only
val `json4s-native`  = "org.json4s" %% "json4s-native"  % "3.6.10"
val `json4s-jackson` = "org.json4s" %% "json4s-jackson" % "3.6.10"
val `spray-json`     = "io.spray"   %% "spray-json"     % "1.3.6"

/////////////////////// PROJECTS /////////////////////////

lazy val root = project
  .in(file("."))
  .aggregate(examples, jsonBenchmark)
  .aggregate(parboiledJVM, parboiledJS)
  .aggregate(parboiledCoreJVM, parboiledCoreJS)
  .settings(commonSettings)
  .settings(releaseSettings)
  .settings(publish / skip := true)

lazy val examples = project
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(parboiledJVM)
  .settings(commonSettings)
  .settings(utestSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(utest.value, `spray-json`)
  )

lazy val bench = inputKey[Unit]("Runs the JSON parser benchmark with a simple standard config")

lazy val jsonBenchmark = project
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(examples)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(`json4s-native`, `json4s-jackson`),
    bench := (run in Compile).partialInput(" -i 10 -wi 10 -f1 -t1").evaluated
  )

lazy val scalaParser = project
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(parboiledJVM)
  .settings(commonSettings)
  .settings(utestSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(shapeless.value, utest.value)
  )

lazy val parboiledJVM = parboiled.jvm
lazy val parboiledJS  = parboiled.js

lazy val parboiled = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin, SbtOsgi)
  .dependsOn(parboiledCore)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(parboiledOsgiSettings)
  .settings(utestSettings)
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
    libraryDependencies ++= Seq(`scala-reflect`.value, shapeless.value, utest.value),
    mappings in (Compile, packageBin) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    mappings in (Compile, packageDoc) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    pomPostProcess := {                                                          // we need to remove the dependency onto the parboiledCore module from the POM
      import scala.xml.transform._
      import scala.xml.{NodeSeq, Node => XNode}

      val filter = new RewriteRule {
        override def transform(n: XNode) = if ((n \ "artifactId").text.startsWith("parboiledcore")) NodeSeq.Empty else n
      }
      new RuleTransformer(filter).transform(_).head
    }
  )

lazy val generateActionOps = taskKey[Seq[File]]("Generates the ActionOps boilerplate source file")

lazy val parboiledCoreJVM = parboiledCore.jvm
lazy val parboiledCoreJS  = parboiledCore.js

lazy val parboiledCore = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("parboiled-core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(utestSettings)
  .jvmSettings(libraryDependencies += scalaCheck.value)
  .jsSettings(libraryDependencies += scalaCheck.value)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(`scala-reflect`.value, shapeless.value, utest.value),
    generateActionOps := ActionOpsBoilerplate((sourceManaged in Compile).value, streams.value),
    (sourceGenerators in Compile) += generateActionOps.taskValue
  )
