scalaVersion := "2.12.4"

lazy val scalene = project

lazy val benchmark = project

libraryDependencies ++= Seq(
  "io.dsimon" %% "microactor" % "0.1.0-SNAPSHOT",
  "ch.qos.logback"               %  "logback-classic"      % "1.2.2",
  "org.slf4j"              %  "slf4j-api"                   % "1.7.6",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

