name := "kolokwium_1"
version := "0.0.1"

scalaVersion := "3.2.2"

scalacOptions ++= Seq(
   "-deprecation",         // Emit warning and location for usages of deprecated APIs.
   "-explain",             // Explain errors in more detail.
   "-feature",             // Emit warning and location for usages of features that should be imported explicitly.
   "-print-lines",         // Show source code line numbers.
   "-unchecked",           // Enable additional warnings where generated code depends on assumptions
   "-Xfatal-warnings",     // Fail the compilation if there are any warnings.
   "-Xmigration",          // Warn about constructs whose behavior may have changed since version.
   "-source:3.0",
   "-encoding", "utf8",
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

/*

  Currently available compiler options and their meanings can be looked-up in:

  https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/dotc/config/ScalaSettings.scala

*/

// Setting someOtherMain as the default "main method":
//
// Compile / run / mainClass := Some("someOtherMain")
