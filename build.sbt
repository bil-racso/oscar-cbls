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

scalacOptions ++= Seq("-P:continuations:enable") //,"-optimize"

seq(assemblySettings: _*)

seq(jacoco.settings : _*)

jarName in assembly := "oscar.jar"


test in assembly := {}


//libraryDependencies += "org.scalatest" % "scalatest" % "1.4.RC2"

//testOptions in Test += Tests.Argument("-oDF")

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test->default"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.2"

testListeners <<= target.map(t => Seq(new eu.henkelmann.sbt.JUnitXmlTestsListener(t.getAbsolutePath)))


//mainClass in (Compile, run) := Some("main.scala.oscar	.dfo.examples.Rosenbrock2D")


parallelExecution in Test := false


