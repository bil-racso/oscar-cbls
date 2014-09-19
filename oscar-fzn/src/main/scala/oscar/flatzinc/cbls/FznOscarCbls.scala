package oscar.flatzinc.cbls

import oscar.flatzinc.parser.Options
import oscar.flatzinc.UnsatException
import oscar.flatzinc.ParsingException
import oscar.flatzinc.NoSuchConstraintException

object FznOscarCbls extends App {
  val mail = "jean-noel.monette@it.uu.se"
  try{
	val opts = new Options(true,args)
	val solutions = new FZCBLSSolver().solve(opts)
  } catch {
    case e: UnsatException => {//thrown when some domain is emptied in preprocessing
      println("====UNSATISFIABLE=====")
    } 
    case e: NoSuchConstraintException => {//might be thrown by the parser or any of the backends
      System.err.println(e.getMessage())
      System.err.println("If this should be an accepted constraint, please report to "+mail)
    }
    case e: Exception => {//catch-all clause...
      System.err.println(e.getMessage())
      System.err.println("Please report the error to "+mail+" with all relevant info and data.")
    }
  }
}