import AssemblyKeys._
import de.johoop.jacoco4sbt._
import JacocoPlugin._


name := "oscar"

version := "1.0"

organization := ""

scalaVersion := "2.9.2"

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

scalacOptions += "-P:continuations:enable"

seq(assemblySettings: _*)

seq(jacoco.settings : _*)

jarName in assembly := "oscar.jar"


test in assembly := {}

mergeStrategy in assembly := { 
  case "reference.conf" =>
    MergeStrategy.concat
  case PathList(ps @ _*) if isReadme(ps.last) || isLicenseFile(ps.last) =>
    MergeStrategy.rename
  case PathList("META-INF", xs @ _*) =>
    (xs map {_.toLowerCase}) match {
      case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
        MergeStrategy.deduplicate
      case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
        MergeStrategy.discard
      case "plexus" :: xs =>
        MergeStrategy.discard
      case "services" :: xs =>
        MergeStrategy.filterDistinctLines
      case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
        MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.deduplicate
    }
  case _ => MergeStrategy.deduplicate
}


//libraryDependencies += "org.scalatest" % "scalatest" % "1.4.RC2"


libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test->default"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.2"

testListeners <<= target.map(t => Seq(new eu.henkelmann.sbt.JUnitXmlTestsListener(t.getAbsolutePath)))


//mainClass in (Compile, run) := Some("main.scala.oscar	.dfo.examples.Rosenbrock2D")


parallelExecution in Test := false


