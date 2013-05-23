import sbt._
import Keys._

object build extends Build {
  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(plugin, main)
  )

  // This subproject contains a Scala compiler plugin that checks for
  // value class boxing after Erasure.
  lazy val plugin = Project(
      id   = "plugin",
      base = file("plugin")
    )

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

  // A regular module with the application code.
  lazy val main = Project(
    id   = "main",
    base = file("main")
  ) settings (pluginSettings: _*)
}