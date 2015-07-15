name := "DSLDI Tutorial"

version := "0.5"

organization := "ETHZ"

scalaVersion := "2.11.7"

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" % "test"
)

crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.3"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
val contVersion = "1.0.2"

autoCompilerPlugins := true

libraryDependencies ++= Seq(
  "org.scala-lang.plugins" %% "scala-continuations-library" % contVersion % "compile"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
     deps :+ compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin" % contVersion cross CrossVersion.full)
}

scalacOptions += "-P:continuations:enable"

val paradiseVersion = "2.0.1"

libraryDependencies ++= (
  if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
  else Nil
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "test")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "compile")

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)




