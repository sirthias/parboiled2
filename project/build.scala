import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

object build extends Build {
  lazy val formatSettings = SbtScalariform.scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := formattingPreferences,
    ScalariformKeys.preferences in Test    := formattingPreferences
  )

  import scalariform.formatter.preferences._
  def formattingPreferences =
    FormattingPreferences()
      .setPreference(RewriteArrowSymbols, true)
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)

  lazy val sharedSettings = Seq(
    scalaVersion := "2.10.3-RC1"
  ) ++ formatSettings

  // configure prompt to show current project
  override lazy val settings = super.settings :+ {
    shellPrompt := { s => Project.extract(s).currentProject.id + " > " }
  }

  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(plugin, main)
  ) settings (sharedSettings: _*)

  // This subproject contains a Scala compiler plugin that checks for
  // value class boxing after Erasure.
  lazy val plugin = Project(
      id   = "plugin",
      base = file("plugin")
    ) settings (sharedSettings: _*)

  // NOTE: If warns are required each time `test` or `compile` is run, then possible settings:
  // https://github.com/alexander-myltsev/parboiled2/commit/52cf666d681d719981fe00485fe25121f8ce9f53#commitcomment-3456157
  // compile in Test <<= (compile in Test).dependsOn(clean)
  // OR
  // cleanFiles in Test <<= Seq(classDirectory in Test).join,
  // cleanKeepFiles in Test := Nil,
  // clean in Test <<= (cleanFiles in Test, cleanKeepFiles in Test) map Defaults.doClean,
  // compile in Test <<= (compile in Test).dependsOn(clean in Test)
  // Scalac command line options to install our compiler plugin.
  lazy val pluginSettings =
    Seq(
      scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
        val addPlugin = "-Xplugin:" + jar.getAbsolutePath
        // add plugin timestamp to compiler options to trigger recompile of
        // main after editing the plugin. (Otherwise a 'clean' is needed.)
        val dummy = "-Jdummy=" + jar.lastModified
        Seq(addPlugin, dummy)
      }
    )

  lazy val main = Project(
    id   = "main",
    base = file("main")
  ).settings(sharedSettings ++ pluginSettings: _*)

  lazy val examples = Project(
    id = "examples",
    base = file("examples")
  ).settings(sharedSettings ++ pluginSettings: _*)
   .dependsOn(main)
}