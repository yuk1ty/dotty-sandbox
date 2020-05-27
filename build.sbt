lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-in-scala-dotty",
    version := "0.1.0",

    scalaVersion := "0.24.0-RC1",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.2" ,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"
  )
