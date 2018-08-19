
val baseSettings = Seq(
  organization := "io.dsimon",
  addCompilerPlugin("io.tryp" % "splain" % "0.3.1" cross CrossVersion.patch),
  scalaVersion := "2.12.6",
  libraryDependencies ++= Seq(
    "ch.qos.logback"               %  "logback-classic"      % "1.2.2",
    "org.slf4j"              %  "slf4j-api"                   % "1.7.6",
    "org.scalactic" %% "scalactic" % "3.0.5",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )
)

lazy val root = (project in file("."))
  .settings(baseSettings)
  .aggregate(`scalene-actor`, `scalene-routing`, scalene, benchmark, examples)

lazy val `scalene-actor` = project
  .settings(baseSettings)

lazy val scalene = project
  .dependsOn(`scalene-actor`)
  .settings(baseSettings)

lazy val scaleneRoutingSettings = baseSettings ++ Seq(
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.2"
  )
)

lazy val `scalene-routing` = project
  .dependsOn(scalene)
  .settings(scaleneRoutingSettings)

val benchmarkSettings = baseSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.json4s"                   %% "json4s-jackson"       % "3.5.3",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.2"
  )
)

lazy val benchmark = project
  .dependsOn(scalene, `scalene-routing`)
  .settings(benchmarkSettings)

lazy val examples = project
  .dependsOn(`scalene-routing`)
  .settings(baseSettings)
