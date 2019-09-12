lazy val oscarUtil = ProjectRef(uri("https://github.com/bil-racso/oscar-util.git"), "oscar-util")

lazy val oscarVisual = ProjectRef(uri("https://github.com/bil-racso/oscar-visual.git"), "oscarVisual")

lazy val oscarCBLS = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "oscarlib",
      scalaVersion := "2.12.10"
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

libraryDependencies ++= Seq()