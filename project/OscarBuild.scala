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
    val buildVersion = "1.2.0.beta"
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
      scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked"/*, "-optimise", "-Xdisable-assertions"*/),
      scalacOptions in Test := Seq("-optimise"),
      testOptions in Test <+= (target in Test) map {
          t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports") ) },
      parallelExecution in Test := false,
      fork in Test := true,
      javaOptions in Test += "-Djava.library.path=../lib:../lib/" + osNativeLibDir,
      unmanagedBase <<= baseDirectory { base => base / "../lib/" }, // unfortunately does not work
      unmanagedClasspath in Compile <+= (baseDirectory) map { bd => Attributed.blank(bd / "../lib/") },
      unmanagedResourceDirectories in Compile <+= baseDirectory( _ / "src" ),
      scalaVersion := buildScalaVersion,
      publishTo := Some(Resolver.url("sbt-release-local", new URL("http://localhost:8081/artifactory/libs-release-local")))
    )
  }
  
  


  object Resolvers {
    val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
    val artifactory = "Artifactory" at "http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"
    val sbtResolvers = Seq (artifactory)
  }

  object Dependencies {

    //val scalatest = "org.scalatest" %% "scalatest" % "2.0.M5b"
    val junit = "junit" % "junit" % "4.8.1" % "test"
    //val scalaswing = "org.scala-lang" % "scala-swing" % "2.11.0-M7"

    // DSL for adding source dependencies ot projects.
    def dependsOnSource(dir: String): Seq[Setting[_]] = {
      import Keys._
      Seq(unmanagedSourceDirectories in Compile <<= (unmanagedSourceDirectories in Compile, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir ) +: srcDirs },
          unmanagedJars in Compile <++= (baseDirectory in ThisBuild) map { base =>
          	val libs = base / dir
          	val dirs = libs // (libs / "batik") +++ (libs / "libtw") +++ (libs / "kiama")
          	(dirs ** "*.jar").classpath
      	   },
          
          unmanagedSourceDirectories in Test <<= (unmanagedSourceDirectories in Test, baseDirectory in ThisBuild) { (srcDirs, base) => (base / dir ) +: srcDirs })
    }
    implicit def p2source(p: Project): SourceDepHelper = new SourceDepHelper(p)
    final class SourceDepHelper(p: Project) {
      def dependsOnSource(dir: String): Project =
        p.settings(Dependencies.dependsOnSource(dir): _*)
    }

  }
  
  import BuildSettings._
  import Dependencies._
  import Resolvers._

  val commonDeps = Seq(/*scalatest,scalaswing,*/junit)
  
 
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
    settings = buildSettings ++ jacoco_settings ++ 
               packSettings ++ unidocSettings ++ 
               Seq (/*resolvers := sbtResolvers,*/ libraryDependencies ++= commonDeps) ++ 
               sbtassembly.Plugin.assemblySettings ++ 
               commonTasks,
    aggregate = Seq(oscarVisual,oscarCp,oscarCbls,oscarFzn,oscarLinprog,oscarDes,oscarDfo),
    dependencies = Seq(oscarCp,oscarCbls,oscarFzn,oscarDes,oscarDfo,oscarLinprog)) dependsOnSource("lib")    
    
  lazy val oscarCbls = Project(
    id = "oscar-cbls",
    base = file("oscar-cbls"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++
    		   sbtassembly.Plugin.assemblySettings ++ 
    		   commonTasks,
    dependencies = Seq(oscarVisual)) dependsOnSource("lib")       
    
  lazy val oscarCp = Project(
    id = "oscar-cp",
    base = file("oscar-cp"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++
    		   sbtassembly.Plugin.assemblySettings ++ 
    		   commonTasks,
    dependencies = Seq(oscarAlgo,oscarVisual)) dependsOnSource("lib")
    
    
  lazy val oscarFzn = Project(
    id = "oscar-fzn",
    base = file("oscar-fzn"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++
    		   sbtassembly.Plugin.assemblySettings ++ 
    		   commonTasks,
    dependencies = Seq(oscarCbls)) dependsOnSource("lib")     
    
  lazy val oscarDes = Project(
    id = "oscar-des",
    base = file("oscar-des"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ 
     		   sbtassembly.Plugin.assemblySettings ++    		   
               commonTasks,
    dependencies = Seq(oscarInvariants)) dependsOnSource("lib")     
    
  lazy val oscarDfo = Project(
    id = "oscar-dfo",
    base = file("oscar-dfo"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ 
               sbtassembly.Plugin.assemblySettings ++ 
               commonTasks,
    dependencies = Seq(oscarAlgebra,oscarVisual,oscarAlgo)) dependsOnSource("lib")       
    
  lazy val oscarLinprog = Project( 
    id = "oscar-linprog",
    base = file("oscar-linprog"),
    settings = buildSettings ++ jacoco_settings ++ Seq(libraryDependencies ++= commonDeps) ++ 
               sbtassembly.Plugin.assemblySettings ++ 
               commonTasks,
    dependencies = Seq(oscarAlgebra)
    ) dependsOnSource("lib")
    

  lazy val oscarAlgo = Project(
    id = "oscar-algo",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) 
               ++ commonTasks,    
    base = file("oscar-algo"),
    dependencies= Seq(oscarUtil,oscarVisual)) dependsOnSource("lib")
    
  lazy val oscarVisual = Project(
    id = "oscar-visual",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ 
               commonTasks,    
    base = file("oscar-visual"),
    dependencies= Seq(oscarUtil)) dependsOnSource("lib")      

  lazy val oscarInvariants = Project(
    id = "oscar-invariants",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-invariants")) dependsOnSource("lib")     
 
 
  lazy val oscarAlgebra = Project(
    id = "oscar-algebra",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-algebra")) dependsOnSource("lib")     

      
    
  lazy val oscarUtil = Project(
    id = "oscar-util",
    settings = buildSettings ++ jacoco_settings ++ Seq (libraryDependencies ++= commonDeps) ++ commonTasks,    
    base = file("oscar-util")) dependsOnSource("lib")       
    

    
  
}
