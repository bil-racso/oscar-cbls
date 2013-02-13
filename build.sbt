import AssemblyKeys._
import de.johoop.jacoco4sbt._
import JacocoPlugin._
import sbt._


name := "oscar"

version := "1.0"

organization := ""

scalaVersion := "2.10.0"

autoCompilerPlugins := true

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
}

//unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(bd / "lib_commercial") }

scalacOptions ++= Seq("-P:continuations:enable") //,"-optimize"

seq(assemblySettings: _*)

seq(jacoco.settings : _*)

// Generate jacoco reports both in XML and HTML
jacoco.reportFormats in jacoco.Config := Seq(XMLReport("utf-8"), HTMLReport("utf-8"))

jarName in assembly := "oscar.jar"

test in assembly := {}


//libraryDependencies += "org.scalatest" % "scalatest" % "1.4.RC2"

//testOptions in Test += Tests.Argument("-oDF")

libraryDependencies += "com.novocode" % "junit-interface" % "0.7" % "test->default"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"


//libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.2"

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  cp filter {x => 
             val v = x.data.getName 
             v == "cplex.jar" || v == "gurobi.jar" || v == "junit-4.10.jar" || v == "scalatest_2.10.0-2.0.M5.jar" }  
}

testOptions in Test <+= (target in Test) map {
  t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports"))
}

TaskKey[Unit]("zipsrc") <<= baseDirectory map { bd => println(bd); IO.zip(Path.allSubpaths(new File(bd + "/src/main/scala")),new File(bd +"/oscar-src.zip"))  }


//mainClass in (Compile, run) := Some("main.scala.oscar	.dfo.examples.Rosenbrock2D")


parallelExecution in Test := false


