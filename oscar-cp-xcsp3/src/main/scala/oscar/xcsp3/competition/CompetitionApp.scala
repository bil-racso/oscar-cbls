package oscar.xcsp3.competition

import org.rogach.scallop.{ScallopConf, ScallopOption}
import oscar.xcsp3.CheckerLib

import scala.util.Random

class CompetitionConf(arguments: Seq[String]) extends ScallopConf(arguments){
  val randomseed: ScallopOption[Int] = opt[Int](default = Some(Random.nextInt(Int.MaxValue))) //A random seed
  val timelimit: ScallopOption[Int] = opt[Int](default = Some(240)) //The time available in seconds
  val memlimit: ScallopOption[Int] = opt[Int](default = Some(1000)) //The memory available in mb
  val nbcore: ScallopOption[Int] = opt[Int](default = Some(1)) //The number of cores available
  val tmpdir: ScallopOption[Int] = opt[Int]() //A temporary directory to write files
  val dir: ScallopOption[Int] = opt[Int]() //The directory containing the program
  val benchname: ScallopOption[String] = trailArg[String]() //The path to the instance file
  verify()
}

abstract class CompetitionApp extends App{
  //Setting up shutdown hook:
  Runtime.getRuntime.addShutdownHook(new Thread{
    override def run() {
      if(!statusPrinted) printStatus()
    }
  })

  val conf = new CompetitionConf(args)

  printComment("seed: " + conf.randomseed())
  printComment("timeout: " + conf.timelimit())
  printComment("memlimit: " + conf.memlimit())
  printComment("nbcore: " + conf.nbcore())

  Random.setSeed(conf.randomseed())
  var statusPrinted = false
  var status = "UNKNOWN"
  var currentSol = ""

  try {
    runSolver(conf)
  }catch{
    case e: Exception =>
      if(!statusPrinted) printStatus()
      printDiagnostic("EXCEPTION", e.getMessage)
      printComment(e.getStackTrace.mkString("\n"))
      Console.flush()
  }

  /**
    * TODO: Parse Instance, launch search and print results
    */
  def runSolver(conf: CompetitionConf): Unit

  // Use the following functions to print your outputs. Only the best solution should be printed.

  //Each time a new best solution is found, this method should be called:
  def updateSol(sol: String, obj: Int): Unit = {
    currentSol = sol
    if(status == "UNKNOWN") status = "SATISFIABLE"
    println("o " + obj)
    Console.flush()
  }

  //Use this only for the last solution:
  //Sol should be a valid instantiation (see rules)
  def printSolution(): Unit = {
    if(currentSol.nonEmpty) {
//      if(new CheckerLib(conf.benchname(), sol).valid)
      println("v " + currentSol.split("\\r?\\n").mkString("\nv "))
//      else{
//        printStatus("UNKNOWN")
//        printDiagnostic("SOL_NOT_VALID")
//        printComment(sol)
//      }
    }
  }

  /**
    * Allowed status:
    * OPTIMUM FOUND: the optimum has been found and proven (already printed in printSolution)
    * SATISFIABLE: a solution been found but cannot be proven optimal (already printed in printSolution)
    * UNSATISFIABLE: the instance is unsatisfiable (i.e. it is proven that no solution exists)
    * UNSUPPORTED: unsupported constraint
    * UNKNOWN: other (no solution has been found or there was a problem, use printDiagnostic to precise information)
    */
  def printStatus(): Unit = {
    println("s " + status)
    if(status == "OPTIMUM FOUND" || status == "SATISFIABLE") printSolution()
    statusPrinted = true
  }

  //For any comment:
  def printComment(com: String): Unit = println("c " + com.split("\\r?\\n").mkString("\nc "))

  //For diagnostic information, name should be a keyword and value no more than one line.
  def printDiagnostic(name: String, value: String): Unit = println("d " + name + " " + value)

  //For diagnostic information, name should be a keyword.
  def printDiagnostic(name: String): Unit = println("d " + name)
}
