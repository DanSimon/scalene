
val baseSettings = Seq(
  organization := "io.dsimon",
  addCompilerPlugin("io.tryp" % "splain" % "0.3.1" cross CrossVersion.patch),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  scalaVersion := "2.12.6",
  version := "0.1.1-SNAPSHOT",
  libraryDependencies ++= Seq(
    "ch.qos.logback"               %  "logback-classic"      % "1.2.2",
    "org.slf4j"              %  "slf4j-api"                   % "1.7.6",
    "org.scalactic" %% "scalactic" % "3.0.5",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  ),
  ThisBuild / githubOwner := "DanSimon",
  ThisBuild / githubRepository := "scalene",
  ThisBuild / githubTokenSource := Some(TokenSource.Environment("GITHUB_SCALENE_TOKEN"))
)

lazy val noPubSettings = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)


lazy val root = (project in file("."))
  .settings(baseSettings)
  .aggregate(`scalene-actor`, `scalene-routing`, scalene, benchmark, examples, `scalene-tests`)

lazy val `scalene-actor` = project
  .settings(baseSettings)

lazy val scalene: Project = project
  .dependsOn(`scalene-actor`)
  .settings(baseSettings)

lazy val scaleneRoutingSettings = baseSettings ++ Seq(
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.2"
  )
)

lazy val `scalene-routing`: Project = project
  .dependsOn(scalene)
  .settings(scaleneRoutingSettings)

lazy val `scalene-tests` = project
  .dependsOn(scalene, `scalene-routing`)
  .settings(baseSettings)
  .settings(noPubSettings)
  .settings(Seq(
    libraryDependencies += "org.scalamock" %% "scalamock" % "4.1.0" % Test
  ))

val benchmarkSettings = baseSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.json4s"                   %% "json4s-jackson"       % "3.5.3",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.2"
  )
)

lazy val benchmark = project
  .dependsOn(scalene, `scalene-routing`)
  .settings(benchmarkSettings)
  .settings(noPubSettings)

lazy val examples = project
  .dependsOn(`scalene-routing`)
  .settings(baseSettings)
  .settings(noPubSettings)
