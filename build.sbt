concurrentRestrictions in Global += Tags.limit(Tags.Test, 1)

val baseSettings = Seq(
  organization := "io.scalene",
  publishMavenStyle := true,
  publishArtifact in Test := false,
  version := "0.1.0",
  scalaVersion := "2.13.1",
  scalacOptions += "-target:jvm-1.8",
  addCompilerPlugin("io.tryp" % "splain" % "0.5.0" cross CrossVersion.patch),
  libraryDependencies ++= Seq(
    "ch.qos.logback"               %  "logback-classic"      % "1.2.2",
    "org.slf4j"              %  "slf4j-api"                   % "1.7.6",
    "org.scalactic" %% "scalactic" % "3.1.0",
    "org.scalatest" %% "scalatest" % "3.1.0" % "test"
  )
)

lazy val noPubSettings = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)


lazy val root = (project in file("."))
  .settings(baseSettings)
  .settings(noPubSettings)
  .aggregate(`scalene-actor`, `scalene-routing`, scalene, benchmark, examples, `scalene-tests`, `scalene-sql`)

lazy val `scalene-actor` = project
  .settings(baseSettings)

lazy val scalene: Project = project
  .dependsOn(`scalene-actor`)
  .settings(baseSettings)

lazy val scaleneRoutingSettings = baseSettings ++ Seq(
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
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
    libraryDependencies += "org.scalamock" %% "scalamock" % "4.4.0" % Test
  ))

val benchmarkSettings = baseSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.json4s"                   %% "json4s-jackson"       % "3.5.5",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.2"
  )
)

lazy val `scalene-sql` = project
  .dependsOn(scalene)
  .settings(baseSettings)
  .settings(Seq(
    libraryDependencies += "org.scalikejdbc" %% "scalikejdbc"        % "3.4.+"
  ))

lazy val benchmark = project
  .dependsOn(scalene, `scalene-routing`, `scalene-sql`)
  .settings(benchmarkSettings)
  .settings(noPubSettings)
  .settings(Seq(
    libraryDependencies += "org.postgresql" % "postgresql"        % "42.2.0"
  ))

lazy val examples = project
  .dependsOn(`scalene-routing`, `scalene-sql`)
  .settings(baseSettings)
  .settings(noPubSettings)
  .settings(Seq(
    libraryDependencies += "org.postgresql" % "postgresql"        % "42.2.0"
  ))
