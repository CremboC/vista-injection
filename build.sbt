import sbt.Keys.libraryDependencies

name := "vista-injection"
organization := "eu.crembo"
version := "1.0.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.1"
// uncomment when/if code is made scala 2.11 compatible
// cross-publish via $ [sbt] + publishLocal -- note the plus
crossScalaVersions := Seq("2.11.8", "2.12.1")

val paradiseVersion    = "3.0.0-M8"
val scalametaVersion   = "1.7.0"
val scalatestVersion   = "3.0.1"
val twitterEvalVersion = "6.42.0"
val shapelessVersion   = "2.3.2"

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  // resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % paradiseVersion cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions += "-Dquasiquote.debug",
  scalacOptions += "-feature",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Define macros in this project.
lazy val macros = (project in file("macros")).settings(
  metaMacroSettings,
  // A dependency on scala.meta is required to write new-style macros, but not
  // to expand such macros.  This is similar to how it works for old-style
  // macros and a dependency on scala.reflect.
  libraryDependencies += "org.scalameta" %% "scalameta" % scalametaVersion,
  libraryDependencies += "org.scalameta" %% "contrib"   % scalametaVersion,
  libraryDependencies += "org.scalactic" %% "scalactic" % scalatestVersion,
  libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test",
  libraryDependencies += "com.twitter"   %% "util-eval" % twitterEvalVersion,
  libraryDependencies += "com.chuusai"   %% "shapeless" % shapelessVersion,
  scalacOptions += "-feature",
  parallelExecution in Test := false
)

lazy val coreSettings: Seq[Def.Setting[_]] = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)

// Use macros in this project.
lazy val core = (project in file("core"))
  .settings(metaMacroSettings ++ coreSettings)
  .dependsOn(macros)

lazy val root = (project in file(".")).aggregate(core, macros)
