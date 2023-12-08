version := "0.1.0-SNAPSHOT"
scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "2.0.19",
  "dev.zio" %% "zio-prelude" % "1.0.0-RC21"
)

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-scala3"
  )
