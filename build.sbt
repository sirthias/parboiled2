import sbtghactions.windows

val Scala2_12 = "2.12.21"
val Scala2_13 = "2.13.18"
val Scala3    = "3.3.8"

val isScala3 = Def.setting(scalaBinaryVersion.value == "3")

ThisBuild / versionScheme := Some("early-semver")

val scalaVersions = Seq(Scala2_12, Scala2_13, Scala3)

val commonSettings = Seq(
  organization := "org.parboiled",
  homepage     := Some(url("http://parboiled.org")),
  description  := "Fast and elegant PEG parsing in Scala - lightweight, easy-to-use, powerful",
  startYear    := Some(2009),
  licenses     := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  (Compile / unmanagedResources) += baseDirectory.value.getParentFile.getParentFile / "LICENSE",
  scmInfo := Some(
    ScmInfo(url("https://github.com/sirthias/parboiled2"), "scm:git:git@github.com:sirthias/parboiled2.git")
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:implicitConversions",
    "-unchecked"
    // "-Ywarn-numeric-widen",
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) =>
        Seq(
          "-language:higherKinds",
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
          "-Xsource:3"
        )
      case Some((2, 13)) =>
        Seq(
          "-Xsource:3-cross",
          "-Xlint:_,-missing-interpolator",
          "-Ywarn-dead-code",
          "-Ywarn-unused:imports,-patvars,-privates,-locals,-implicits,-explicits",
          "-Ycache-macro-class-loader:last-modified",
          "-Ybackend-parallelism",
          "8"
        )
      case Some((3, _)) =>
        Nil
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
  evictionErrorLevel := Level.Warn
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
  publishConfiguration      := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
  developers                := List(
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

val utest           = Def.setting("com.lihaoyi" %%% "utest" % "0.8.4" % Test)
val scalaCheck      = Def.setting("org.scalacheck" %%% "scalacheck" % "1.19.0" % Test)
val `scala-reflect` = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)

// benchmarks and examples only
val `json4s-native`  = "io.github.json4s" %% "json4s-native"  % "4.1.1"
val `json4s-jackson` = "io.github.json4s" %% "json4s-jackson" % "4.1.1"
val `spray-json`     = "io.spray"         %% "spray-json"     % "1.3.6"

/////////////////////// PROJECTS /////////////////////////

lazy val root = project
  .in(file("."))
  .aggregate(examples.projectRefs *)
  .aggregate(jsonBenchmark.projectRefs *)
  .aggregate(scalaParser.projectRefs *)
  .aggregate(parboiled.projectRefs *)
  .aggregate(parboiledCore.projectRefs *)
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(
    name := "parboiled2-root"
  )

lazy val examples = projectMatrix
  .defaultAxes(VirtualAxis.jvm)
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(parboiled)
  .jvmPlatform(scalaVersions)
  .settings(commonSettings)
  .settings(utestSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(utest.value, `spray-json`)
  )

lazy val bench = inputKey[Unit]("Runs the JSON parser benchmark with a simple standard config")

lazy val jsonBenchmark = projectMatrix
  .defaultAxes(VirtualAxis.jvm)
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(examples)
  .jvmPlatform(scalaVersions)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(`json4s-native`, `json4s-jackson`),
    bench := (Compile / run).partialInput(" -i 10 -wi 10 -f1 -t1").evaluated
  )

lazy val scalaParser = projectMatrix
  .defaultAxes(VirtualAxis.jvm)
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(parboiled)
  .jvmPlatform(scalaVersions)
  .settings(commonSettings)
  .settings(utestSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(utest.value)
  )

lazy val parboiled = projectMatrix
  .defaultAxes()
  .enablePlugins(AutomateHeaderPlugin, SbtOsgi)
  .dependsOn(parboiledCore)
  .settings(commonSettings)
  .settings(publishingSettings)
  .settings(parboiledOsgiSettings)
  .settings(utestSettings)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      (Compile / packageBin / packageOptions) += Package.ManifestAttributes(
        "Automatic-Module-Name" -> "org.parboiled2.parboiled"
      )
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
    )
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings
    )
  )
  .settings(
    libraryDependencies ++= {
      if (isScala3.value) Seq(utest.value)
      else Seq(`scala-reflect`.value, utest.value)
    },
    Seq(packageBin, packageSrc, packageDoc).map { packageTask =>
      (Compile / packageTask / mappings) ++= Def.taskDyn {
        val Seq(p) = parboiledCore.allProjects().filter(_._2 == virtualAxes.value).map(_._1)
        Def.task {
          (p / Compile / packageTask / mappings).value
        }
      }.value
    },
    (Compile / packageBin / mappings) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    (Compile / packageDoc / mappings) ~= (_.groupBy(_._2).toSeq.map(_._2.head)), // filter duplicate outputs
    projectDependencies := projectDependencies.value.filterNot(_.name.equalsIgnoreCase("parboiledcore"))
  )

lazy val generateActionOps = taskKey[Seq[File]]("Generates the ActionOps boilerplate source file")

lazy val parboiledCore = projectMatrix
  .defaultAxes()
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
  .jvmPlatform(
    scalaVersions,
    Def.settings(
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
    )
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings
    )
  )

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(
    RefPredicate.StartsWith(Ref.Tag("v"))
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
  WorkflowStep.Use(
    ref = UseRef.Public("egor-tensin", "setup-clang", "v2"),
    cond = Some("runner.os == 'Windows'"),
    params = Map(
      "version"  -> "20.1.7",
      "platform" -> "x64"
    )
  ),
  WorkflowStep.Sbt(
    List("headerCheckAll"),
    name = Some("Header check"),
    cond = {
      // Header check only needs to be run for one JVM
      val jVersion = (ThisBuild / githubWorkflowJavaVersions).value.head.render
      val uOs      = (ThisBuild / githubWorkflowOSes).value.find(_.contains("ubuntu")).head
      Some(
        "${{ matrix.java=='" + jVersion + "' && matrix.os=='" + uOs + "' }}"
      )
    }
  ),
  WorkflowStep.Sbt(
    List("Test/compile", "test", "testOnly scalaparser.SnippetSpec"),
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
