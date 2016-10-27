name := "vista-injection"
//organization := ""
//version := "2.0.0"
//
//scalaVersion in ThisBuild := "2.11.8"
//run <<= run in Compile in core

lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.8",
  scalacOptions := Seq("-Xplugin-require:macroparadise", "-unchecked", "-deprecation", "-language:experimental.macros", "-Xlog-free-terms"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

lazy val macros = (project in file("macros"))
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
  .settings(commonSettings:_*)

lazy val core = (project in file("core"))
  .settings(commonSettings:_*)
  .dependsOn(macros)
