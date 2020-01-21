
val baseSettings = Seq(
  organization := "io.dsimon",
  scalaVersion := "2.12.10",
  addCompilerPlugin("io.tryp" % "splain" % "0.5.0" cross CrossVersion.patch),
  libraryDependencies ++= Seq(
    "ch.qos.logback"               %  "logback-classic"      % "1.2.2",
    "org.slf4j"              %  "slf4j-api"                   % "1.7.6",
    "org.scalactic" %% "scalactic" % "3.0.5",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )
)

lazy val root = (project in file("."))
  .settings(baseSettings)
  .aggregate(`scalene-actor`, `scalene-routing`, scalene, benchmark, examples, `scalene-tests`, `scalene-sql`)

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
  .settings(Seq(
    libraryDependencies += "org.scalamock" %% "scalamock" % "4.1.0" % Test
  ))

val benchmarkSettings = baseSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.json4s"                   %% "json4s-jackson"       % "3.5.3",
    "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.2"
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
  .settings(Seq(
    libraryDependencies += "org.postgresql" % "postgresql"        % "42.2.0"    
  ))

lazy val examples = project
  .dependsOn(`scalene-routing`, `scalene-sql`)
  .settings(baseSettings)
  .settings(Seq(
    libraryDependencies += "org.postgresql" % "postgresql"        % "42.2.0"    
  ))
