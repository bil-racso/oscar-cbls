package oscar

import sbt._
import sbt.Keys._
import java.lang.Boolean.getBoolean
import de.johoop.jacoco4sbt.JacocoPlugin._
import xerial.sbt.Pack._
import sbtunidoc.Plugin._


object OscarBuild extends Build {

  object BuildSettings {
    val buildOrganization = "oscar"
    val buildVersion = "3.0.0.beta"
    val buildScalaVersion = "2.11.0"
    val buildSbtVersion= "0.13.0"

    val osNativeLibDir = (sys.props("os.name"), sys.props("os.arch")) match {
      case (os, arch) if os.contains("Mac") && arch.endsWith("64") => "macos64"
      case (os, arch) if os.contains("Linux") && arch.endsWith("64") => "linux64"
      case (os, arch) if os.contains("Windows") && arch.endsWith("32") => "windows32"
      case (os, arch) if os.contains("Windows") && arch.endsWith("64") => "windows64"
      case (os, arch) => sys.error("Unsupported OS [${os}] Architecture [${arch}] combo, OscaR currently supports macos64, linux64, windows32, windows64")
    }

    val buildSettings = Defaults.defaultSettings ++ Seq(
      organization := buildOrganization,
      version := buildVersion,
      scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked", "-Xdisable-assertions"),
      scalacOptions in Test := Seq("-optimise"),
      testOptions in Test <+= (target in Test) map {
          t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports") ) },
      parallelExecution in Test := false,
      fork in Test := true,
      javaOptions in Test += "-Djava.library.path=../lib:../lib/" + osNativeLibDir,
      scalaVersion := buildScalaVersion,
      publishTo := Some(Resolver.url("sbt-release-local", new URL("http://localhost:8081/artifactory/libs-release-local"))),
      unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "main" / "examples"
    )
  }
  
  


  object Resolvers {
    val xypron = "Xypron Release" at "http://rsync.xypron.de/repository/"
    val leadoperations = "AWS S3 Release Repository" at "http://maven.leadoperations.co/release"
    val cogcomp = "Cognitive Computation Group" at "http://cogcomp.cs.illinois.edu/m2repo/"
  }

  object Dependencies {

    // TODO: Use `latest.milestone` where possible

    // Regular libraries
    val antlr4Runtime = "org.antlr" % "antlr4-runtime" % "4.5.1"
    val glpk = "org.gnu.glpk" % "glpk-java" % "1.1.0"
    val gurobi = "gurobi" % "gurobi" % "5.0.1"
    val lpsolve = "lpsolve" % "lpsolve" % "5.5.2"
    val jcommon = "org.jfree" % "jcommon" % "1.0.17"
    val jfreechart = "org.jfree" % "jfreechart" % "1.0.14"
    val jsci = "net.sf.jsci" % "jsci" % "1.2"
    val scalaParserCombinators = "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.1"
    val scalaXml = "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.1"
    val scalaSwing = "org.scala-lang" % "scala-swing" % "2.11.0-M7"
    val swingx = "org.swinglabs" % "swingx" % "1.6.1"
    val swingxWs = "org.swinglabs" % "swingx-ws" % "1.0"
    val xmlApisExt = "xml-apis" % "xml-apis-ext" % "1.3.04"
    
    // Test libraries
    val junit = "junit" % "junit" % "4.8.1" % Test
    val scalaCheck = "org.scalacheck" % "scalacheck_2.11" % "1.11.3" % Test
    val scalaTest = "org.scalatest" % "scalatest_2.11" % "2.2.5" % Test
  }
  
  import BuildSettings._
  import Dependencies._
  import Resolvers._
  import UnidocKeys._

  val commonDeps = Seq(junit, scalaCheck, scalaTest)
  
 
  TaskKey[Unit]("zipsrc") <<= baseDirectory map { bd => println(bd); IO.zip(Path.allSubpaths(new File(bd + "/src/main/scala")),new File(bd +"/oscar-src.zip"))  }
    
  val hello = TaskKey[Unit]("hello", "hello documentation")
  
  val helloTask = hello := {
    println("Hello World")
  }
  
    
  val printLinprog = TaskKey[Unit]("printLinprog", "printLinProg")
  
  val printLinprogTask = printLinprog := {
    println("base "+baseDirectory)
    
    println(baseDirectory.map { base => base })
  }  
  
  val zipsrc = TaskKey[Unit]("zipsrc","zip the source") <<= baseDirectory map { bd => println(bd); IO.zip(Path.allSubpaths(new File(bd + "/src/main/scala")),new File(bd +"/oscar-src.zip"))  }

  val foo = TaskKey[Unit]("foo","foo task") <<= baseDirectory map { bd => println(bd)}

  val commonTasks = Seq(helloTask,foo,zipsrc,printLinprogTask)
  
  lazy val jacoco_settings = Defaults.defaultSettings ++ Seq(jacoco.settings: _*)

  lazy val oscar = Project(
    id = "oscar",
    base = file("."),
    settings =
      buildSettings ++
      jacoco_settings ++ 
      packSettings ++
      unidocSettings ++ 
      Seq (libraryDependencies ++= commonDeps) ++ 
      sbtassembly.Plugin.assemblySettings ++ 
      (unidocProjectFilter in (ScalaUnidoc, unidoc) :=
        inAnyProject -- inProjects(oscarFzn)) ++
      commonTasks,
    aggregate = Seq(oscarVisual,oscarCp,oscarCbls/*,oscarFzn*/,oscarLinprog,oscarDes,oscarDfo),
    dependencies = Seq(oscarCp,oscarCbls/*,oscarFzn*/,oscarDes,oscarDfo,oscarLinprog))

  lazy val oscarCbls = Project(
    id = "oscar-cbls",
    base = file("oscar-cbls"),
    settings =
      buildSettings ++
      jacoco_settings ++
      packAutoSettings ++
      Seq(
        libraryDependencies ++= commonDeps :+ scalaSwing,
        unmanagedSourceDirectories in Compile += baseDirectory.value / "src/main/examples",
        packGenerateWindowsBatFile := false
      ) ++
      sbtassembly.Plugin.assemblySettings ++ 
      commonTasks,
    dependencies = Seq(oscarVisual))

  lazy val oscarCp = Project(
    id = "oscar-cp",
    base = file("oscar-cp"),
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq(libraryDependencies ++= commonDeps :+ scalaParserCombinators) ++
      sbtassembly.Plugin.assemblySettings ++
      commonTasks,
    dependencies = Seq(oscarAlgo,oscarVisual))

  lazy val oscarFzn = Project(
    id = "oscar-fzn",
    base = file("oscar-fzn"),
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq(libraryDependencies ++= commonDeps :+ antlr4Runtime) ++
      sbtassembly.Plugin.assemblySettings ++ 
      commonTasks,
    dependencies = Seq(oscarCbls))

  lazy val oscarDes = Project(
    id = "oscar-des",
    base = file("oscar-des"),
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq(libraryDependencies ++= commonDeps :+ jsci) ++
      sbtassembly.Plugin.assemblySettings ++
      commonTasks,
    dependencies = Seq(oscarInvariants))

  lazy val oscarDfo = Project(
    id = "oscar-dfo",
    base = file("oscar-dfo"),
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq(libraryDependencies ++= commonDeps :+ jcommon :+ jfreechart) ++
      sbtassembly.Plugin.assemblySettings ++
      commonTasks,
    dependencies = Seq(oscarAlgebra,oscarVisual,oscarAlgo))

  lazy val oscarLinprog = Project( 
    id = "oscar-linprog",
    base = file("oscar-linprog"),
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq(
        resolvers ++= Seq(xypron, leadoperations, cogcomp),
        libraryDependencies ++= commonDeps :+ glpk :+ gurobi :+ lpsolve
      ) ++ 
      sbtassembly.Plugin.assemblySettings ++ 
      commonTasks,
    dependencies = Seq(oscarAlgebra))

  lazy val oscarAlgo = Project(
    id = "oscar-algo",
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq (libraryDependencies ++= commonDeps) ++
      commonTasks,    
    base = file("oscar-algo"),
    dependencies= Seq(oscarUtil,oscarVisual))

  lazy val oscarVisual = Project(
    id = "oscar-visual",
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq (libraryDependencies ++= commonDeps :+ jfreechart :+ swingx :+ swingxWs) ++
      commonTasks,
    base = file("oscar-visual"),
    dependencies= Seq(oscarUtil))

  lazy val oscarInvariants = Project(
    id = "oscar-invariants",
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq (libraryDependencies ++= commonDeps) ++
      commonTasks,    
    base = file("oscar-invariants"))

  lazy val oscarAlgebra = Project(
    id = "oscar-algebra",
    settings =
      buildSettings ++
      jacoco_settings ++
      Seq (libraryDependencies ++= commonDeps) ++
      commonTasks,    
    base = file("oscar-algebra"))

  lazy val oscarUtil = Project(
    id = "oscar-util",
    settings = buildSettings ++
      jacoco_settings ++
      Seq (libraryDependencies ++= commonDeps :+ scalaXml) ++
      commonTasks,    
    base = file("oscar-util"))
}
