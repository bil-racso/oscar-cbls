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
    val buildVersion = "1.0"
    val buildScalaVersion = "2.10.0"
    val buildSbtVersion= "0.13.0"
    val buildSettings = Defaults.defaultSettings ++ Seq(
      organization := buildOrganization,
      version := buildVersion,
      scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked","-P:continuations:enable", "-Xdisable-assertions"),
      
      libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
      	deps :+ compilerPlugin("org.scala-lang.plugins" % "continuations" % ver)
      },
      testOptions in Test <+= (target in Test) map {
          t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports")) },
      parallelExecution in Test := false,    
      unmanagedBase <<= baseDirectory { base => base / "../lib/" }, // unfortunately does not work
      unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(bd / "../lib/") },
      scalaVersion := buildScalaVersion)
  }

  object Resolvers {
    val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
    val artifactory = "Artifactory" at "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"
    val sbtResolvers = Seq (artifactory)
  }

  object Dependencies {

    val scalatest =  "org.scalatest" %% "scalatest" % "2.0.M5b" 
    val junit = "junit" % "junit" % "4.8.1" % "test"
    val scalaswing = "org.scala-lang" % "scala-swing" % "2.10.0"

  }
  
  import BuildSettings._
  import Dependencies._
  import Resolvers._

  val commonDeps = Seq(scalatest,junit,scalaswing)
  
 
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
  
  //
  lazy val jacoco_settings = Defaults.defaultSettings ++ Seq(jacoco.settings: _*)
  //jacoco.reportFormats in jacoco.Config := Seq(XMLReport("utf-8"), HTMLReport("utf-8"))
  
  

  
  lazy val oscar = Project(
    id = "oscar",
    base = file("."),
    //
    settings = buildSettings ++ jacoco_settings ++ packSettings ++ unidocSettings ++ Seq (/*resolvers := sbtResolvers,*/ libraryDependencies ++= commonDeps) ++ commonTasks,
    aggregate = Seq(oscarVisual,oscarCp,oscarCbls,oscarLinprog,oscarDes,oscarDfo))


  lazy val oscarExamples = Project(
    id = "oscar-examples",
    base = file("oscar-examples"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ commonTasks,
    dependencies = Seq(oscarVisual,oscarCp,oscarCbls,oscarLinprog,oscarDes,oscarDfo))         
    
    
  lazy val oscarCbls = Project(
    id = "oscar-cbls",
    base = file("oscar-cbls"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ commonTasks,
    dependencies = Seq(oscarVisual))       
    
  lazy val oscarCp = Project(
    id = "oscar-cp",
    base = file("oscar-cp"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ commonTasks,
    dependencies = Seq(oscarSearch,oscarReversible,oscarVisual,oscarAlgo))  
    
  lazy val oscarDes = Project(
    id = "oscar-des",
    base = file("oscar-des"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ commonTasks,
    dependencies = Seq(oscarInvariants))       
    
  lazy val oscarDfo = Project(
    id = "oscar-dfo",
    base = file("oscar-dfo"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ commonTasks,
    dependencies = Seq(oscarAlgebra,oscarVisual))        
    
  lazy val oscarLinprog = Project(
    id = "oscar-linprog",
    base = file("oscar-linprog"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ commonTasks,
    dependencies = Seq(oscarAlgebra))   

  oscarLinprog.settings(unmanagedBase <<= baseDirectory { base => base.asFile.getParentFile() } )    
    
   /* 
  oscarLinprog.settings(unmanagedBase <<= baseDirectory { 
    println("yahoo")
    base => {
      println(base.asFile.getParent())
      base.asFile.getParentFile() } 
    })
  */
    
  oscarLinprog.settings(unmanagedBase := file("libo"))
  
    
  lazy val oscarSearch = Project(
    id = "oscar-search",
    base = file("oscar-search"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ commonTasks,
    dependencies = Seq(oscarReversible,oscarVisual))
    
  lazy val oscarVisual = Project(
    id = "oscar-visual",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-visual"),
    dependencies= Seq(oscarUtil))        

  lazy val oscarInvariants = Project(
    id = "oscar-invariants",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-invariants"))      
 
  lazy val oscarReversible = Project(
    id = "oscar-reversible",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-reversible"))       

  lazy val oscarAlgebra = Project(
    id = "oscar-algebra",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-algebra"))       

  lazy val oscarAlgo = Project(
    id = "oscar-algo",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-algo"))         
    
  lazy val oscarUtil = Project(
    id = "oscar-util",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-util"))        
    

    
  
}
