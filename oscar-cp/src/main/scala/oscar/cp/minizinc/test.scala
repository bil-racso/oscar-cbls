package oscar.cp.minizinc

import java.io.FileReader
import oscar.cp.core.NoSolutionException

object test extends NewParser with App{

  
    val file = Array("minizinc/unit/test1.fzn")
  
  
    val opts = new Options(file/*args*/)

    try {
      myParseAll(opts)
      println("here")

    } catch {
      case e: NoSolutionException => println("=====UNSATISFIABLE=====")
      case e: Throwable => {
        println(e)
        println(e.printStackTrace())
        throw new Exception("adding the constraint failed")
      }
    }
}
