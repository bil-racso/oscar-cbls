lazy val oscarUtil = ProjectRef(uri("https://github.com/bil-racso/oscar-util.git"), "oscar-util")

lazy val oscarVisual = ProjectRef(uri("https://github.com/bil-racso/oscar-visual.git"), "oscarVisual")

lazy val oscarCBLS = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "oscarlib",
      scalaVersion := "2.13.2",
      version := "5.0.0"
	)),
    name := "oscar-cbls").
  dependsOn(oscarUtil).
  dependsOn(oscarVisual)

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xdisable-assertions",
  "-language:implicitConversions",
  "-language:postfixOps"
)

libraryDependencies ++= Seq(
  "org.locationtech.jts" % "jts-core" % "1.16.1",
  "org.scalatest" %% "scalatest" % "3.1.2" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.0.0" % Test
)

// Cross-version for parallel collections
libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major <= 12 =>
      Seq()
    case _ =>
      Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
  }
}