import sbt.Keys.libraryDependencies

name := "vista-injection"
scalaVersion in ThisBuild := "2.12.1"

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.url("scalameta", url("http://dl.bintray.com/scalameta/maven"))(
    Resolver.ivyStylePatterns),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions += "-Dquasiquote.debug",
  scalacOptions += "-feature",
  scalacOptions += "-language:implicitConversions",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil, // macroparadise doesn't work with scaladoc yet.
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Define macros in this project.
lazy val macros = (project in file("macros")).settings(
  metaMacroSettings,
  // A dependency on scala.meta is required to write new-style macros, but not
  // to expand such macros.  This is similar to how it works for old-style
  // macros and a dependency on scala.reflect.
  libraryDependencies += "org.scalameta" %% "scalameta"   % "1.6.0",
  libraryDependencies += "org.scalameta" %% "contrib"     % "1.7.0-385-2c036181",
  libraryDependencies += "org.scalactic" %% "scalactic"   % "3.0.1",
  libraryDependencies += "org.scalatest" %% "scalatest"   % "3.0.1" % "test",
  libraryDependencies += "org.scalaz"    %% "scalaz-core" % "7.2.10",
  scalacOptions += "-feature"
)

// Use macros in this project.
lazy val core = (project in file("core"))
  .settings(metaMacroSettings)
  .dependsOn(macros)
