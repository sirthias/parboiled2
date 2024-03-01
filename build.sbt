import sbtcrossproject.CrossPlugin.autoImport._
import sbtghactions.windows

val Scala2_12 = "2.12.19"
val Scala2_13 = "2.13.13"
val Scala3    = "3.4.0"

val isScala3 = Def.setting(scalaBinaryVersion.value == "3")

ThisBuild / versionScheme      := Some("early-semver")
ThisBuild / scalaVersion       := Scala3
ThisBuild / crossScalaVersions := Seq(Scala2_12, Scala2_13, Scala3)

val commonSettings = Seq(
  organization := "org.parboiled",
  homepage     := Some(new URL("http://parboiled.org")),
  description  := "Fast and elegant PEG parsing in Scala - lightweight, easy-to-use, powerful",
  startYear    := Some(2009),
  licenses     := Seq("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  (Compile / unmanagedResources) += baseDirectory.value.getParentFile.getParentFile / "LICENSE",
  scmInfo := Some(
    ScmInfo(url("https://github.com/sirthias/parboiled2"), "scm:git:git@github.com:sirthias/parboiled2.git")
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:_",
    "-unchecked"
    // "-Ywarn-numeric-widen",
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
          "-Xlint:_,-missing-interpolator",
          "-Ywarn-dead-code",
          "-Xsource:2.13" // new warning: deprecate assignments in argument position
        )
      case Some((2, 13)) =>
        Seq(
          "-Xlint:_,-missing-interpolator",
          "-Ywarn-dead-code",
          "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
          "-Ycache-macro-class-loader:last-modified",
          "-Ybackend-parallelism",
          "8"
        )
      case Some((3, _)) =>
        Seq("-language:implicitConversions")
      case x => sys.error(s"unsupported scala version: $x")
    }
  },
  Compile / scalacOptions ++= {
    if (insideCI.value) {
      val log = sLog.value
      log.info("Running in CI, enabling Scala2 optimizer")
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) | Some((2, 13)) =>
          Seq(
            "-opt-inline-from:org.parboiled2.**",
            "-opt:l:inline"
          )
        case Some((3, _)) =>
          Seq.empty // // Optimizer not yet available for Scala3, see https://docs.scala-lang.org/overviews/compiler-options/optimizer.html
        case x => sys.error(s"unsupported scala version: $x")
      }
    } else Seq.empty
  },
  Compile / console / scalacOptions ~= (_ filterNot (o => o == "-Ywarn-unused-import" || o == "-Xfatal-warnings")),
  Test / console / scalacOptions ~= (_ filterNot (o => o == "-Ywarn-unused-import" || o == "-Xfatal-warnings")),
  Compile / doc / scalacOptions += "-no-link-warnings",
  sourcesInBase := false,
  // file headers
  headerLicense := Some(HeaderLicense.ALv2("2009-2019", "Mathias Doenitz"))
)

lazy val nativeSettings = Def.settings(
)

lazy val scalajsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule).withSourceMap(false)),
  Global / scalaJSStage := FastOptStage,
  scalacOptions ~= { _.filterNot(_ == "-Ywarn-dead-code") }
)

lazy val publishingSettings = Seq(
  publishMavenStyle         := true,
  Test / publishArtifact    := false,
  pomIncludeRepository      := (_ => false),
  publishTo                 := sonatypePublishToBundle.value,
  publishConfiguration      := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  developers := List(
    Developer("sirthias", "Mathias Doenitz", "devnull@bullet.io", url("https://github.com/sirthias")),
    Developer("alexander-myltsev", "Alexander Myltsev", "", url("http://www.linkedin.com/in/alexandermyltsev"))
  )
)

val utestSettings = Seq(testFrameworks := Seq(new TestFramework("utest.runner.Framework")))

lazy val parboiledOsgiSettings = osgiSettings ++ Seq(
  OsgiKeys.exportPackage  := Seq("org.parboiled2.*;version=${Bundle-Version}"),
  OsgiKeys.privatePackage := Seq()
)

/////////////////////// DEPENDENCIES /////////////////////////

val utest           = Def.setting("com.lihaoyi" %%% "utest" % "0.8.2" % Test)
val scalaCheck      = Def.setting("org.scalacheck" %%% "scalacheck" % "1.17.0" % Test)
val `scala-reflect` = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)

// benchmarks and examples only
val `json4s-native`  = "org.json4s" %% "json4s-native"  % "4.0.7"
val `json4s-jackson` = "org.json4s" %% "json4s-jackson" % "4.0.7"
val `spray-json`     = "io.spray"   %% "spray-json"     % "1.3.6"

/////////////////////// PROJECTS /////////////////////////

lazy val root = project
  .in(file("."))
  .aggregate(examples, jsonBenchmark)
  .aggregate(parboiledJVM, parboiledJS, parboiledNative)
  .aggregate(parboiledCoreJVM, parboiledCoreJS, parboiledCoreNative)
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(
    name := "parboiled2-root"
  )

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
    bench := (Compile / run).partialInput(" -i 10 -wi 10 -f1 -t1").evaluated
  )

lazy val scalaParser = project
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(parboiledJVM)
  .settings(commonSettings)
  .settings(utestSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(utest.value)
  )

lazy val parboiledJVM    = parboiled.jvm
lazy val parboiledJS     = parboiled.js
lazy val parboiledNative = parboiled.native

lazy val parboiled = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(AutomateHeaderPlugin, SbtOsgi)
  .dependsOn(parboiledCore)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(parboiledOsgiSettings)
  .settings(utestSettings)
  .jvmSettings(
    (Compile / packageBin / mappings) ++= (parboiledCoreJVM.project / Compile / packageBin / mappings).value,
    (Compile / packageSrc / mappings) ++= (parboiledCoreJVM.project / Compile / packageSrc / mappings).value,
    (Compile / packageDoc / mappings) ++= (parboiledCoreJVM.project / Compile / packageDoc / mappings).value,
    (Compile / packageBin / packageOptions) += Package.ManifestAttributes(
      "Automatic-Module-Name" -> "org.parboiled2.parboiled"
    )
  )
  .jsSettings(
    (Compile / packageBin / mappings) ++= (parboiledCoreJS.project / Compile / packageBin / mappings).value,
    (Compile / packageSrc / mappings) ++= (parboiledCoreJS.project / Compile / packageSrc / mappings).value,
    (Compile / packageDoc / mappings) ++= (parboiledCoreJS.project / Compile / packageDoc / mappings).value
  )
  .nativeSettings(
    (Compile / packageBin / mappings) ++= (parboiledCoreNative.project / Compile / packageBin / mappings).value,
    (Compile / packageSrc / mappings) ++= (parboiledCoreNative.project / Compile / packageSrc / mappings).value,
    (Compile / packageDoc / mappings) ++= (parboiledCoreNative.project / Compile / packageDoc / mappings).value
  )
  .settings(
    libraryDependencies ++= {
      if (isScala3.value) Seq(utest.value)
      else Seq(`scala-reflect`.value, utest.value)
    },
    (Compile / packageBin / mappings) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    (Compile / packageDoc / mappings) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    projectDependencies := projectDependencies.value.filterNot(_.name.equalsIgnoreCase("parboiledcore"))
  )
  .nativeSettings(nativeSettings)

lazy val generateActionOps = taskKey[Seq[File]]("Generates the ActionOps boilerplate source file")

lazy val parboiledCoreJVM    = parboiledCore.jvm
lazy val parboiledCoreJS     = parboiledCore.js
lazy val parboiledCoreNative = parboiledCore.native

lazy val parboiledCore = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("parboiled-core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(utestSettings)
  .settings(libraryDependencies += scalaCheck.value)
  .settings(
    publish / skip := true,
    libraryDependencies ++= {
      if (isScala3.value) Seq(utest.value)
      else Seq(`scala-reflect`.value, utest.value)
    },
    generateActionOps := ActionOpsBoilerplate((Compile / sourceManaged).value, streams.value),
    Compile / sourceGenerators += generateActionOps.taskValue
  )
  .nativeSettings(nativeSettings)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(
    RefPredicate.StartsWith(Ref.Tag("v")),
    RefPredicate.Equals(Ref.Branch("master"))
  )

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    commands = List("ci-release"),
    name = Some("Publish project"),
    env = Map(
      "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8"), JavaSpec.temurin("21"))
ThisBuild / githubWorkflowOSes         := Seq("ubuntu-latest", "windows-latest")

ThisBuild / githubWorkflowWindowsPagefileFix := Some(
  windows.PagefileFix("4GB", "16GB")
)

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    List("headerCheckAll", "scalaParser/headerCheckAll"),
    name = Some("Header check"),
    cond = {
      // Header check only needs to be run for one JVM, os along with one version of Scala
      val jVersion = (ThisBuild / githubWorkflowJavaVersions).value.head.render
      val sVersion = (ThisBuild / crossScalaVersions).value.head
      val uOs      = (ThisBuild / githubWorkflowOSes).value.find(_.contains("ubuntu")).head
      Some(
        "${{ matrix.java=='" + jVersion + "' && matrix.scala=='" + sVersion + "' && matrix.os=='" + uOs + "' }}"
      )
    }
  ),
  WorkflowStep.Sbt(
    List("Test/compile", "test", "scalaParser/testOnly scalaparser.SnippetSpec"),
    name = Some("Build project")
  )
)

ThisBuild / githubWorkflowGeneratedCI ~= {
  _.map {
    case x if x.id == "publish" =>
      x.copy(cond = x.cond.map(_ + " && (github.repository_owner == 'sirthias')"))
    case x =>
      x
  }
}
