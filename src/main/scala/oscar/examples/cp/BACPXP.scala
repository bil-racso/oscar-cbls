/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import oscar.cp.constraints._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._
import java.io.FileWriter
import java.util.{Date, Locale}
import java.text.DateFormat
import java.text.DateFormat._

/**
 * Balanced Academic Curriculum Problem
 * The BACP is to design a balanced academic curriculum by assigning periods to courses in a way that 
 * the academic load of each period is balanced, i.e., as similar as possible . The curriculum must obey the following administrative and academic regulations: 
 * Academic curriculum: an academic curriculum is defined by a set of courses and a set of prerequisite relationships among them. 
 * Number of periods: courses must be assigned within a maximum number of academic periods. 
 * Academic load: each course has associated a number of credits or units that represent the academic effort required to successfully follow it. 
 * Prerequisites: some courses can have other courses as prerequisites. 
 * Minimum academic load: a minimum amount of academic credits per period is required to consider a student as full time. 
 * Maximum academic load: a maximum amount of academic credits per period is allowed in order to avoid overload. 
 * Minimum number of courses: a minimum number of courses per period is required to consider a student as full time. 
 * Maximum number of courses: a maximum number of courses per period is allowed in order to avoid overload. 
 * The goal is to assign a period to every course in a way that 
 * - the minimum and maximum academic load for each period, 
 * - the minimum and maximum number of courses for each period, 
 * - and the prerequisite relationships are satisfied. 
 * An optimal balanced curriculum balances academic load for all periods. 
 * @author Pierre Schaus pschaus@gmail.com
 */
object BACPXP extends App{
	final val SHAW 	= 0
	final val CURRENT	= 1
	final val EXTENDED = 2
	
	final val TIME_LIMIT = 15
	final val LOG_FILE = "bench_BACPCP.txt"

	log("file\t timeLimit \t\t\t Shaw \t Shaw+BinPackingFlow \t  Shaw+BinPackingFlowExtended"
	    +"\n result : (isOptimal, best value, backtracks, time) \n")
	//println(solveFile("data/bacp/instances12/inst9.txt",EXTENDED))
	    
	for(timeLimit <- List(15, 30, 60, 120, 180, 240, 300))
	{
	  val rs = new ResultsStats()
		(1 to 99).par.map("inst" + _ +".txt").foreach(logFileResults(_,rs,timeLimit))
		log("============= stats for timeLimit:"+timeLimit + "\t" + rs.results.mkString("\t") + "\n")
	}
	  
	
	def log(line : String, logFile : String = LOG_FILE) =
	  {
	    this.synchronized 
	    {
		     val fw = new FileWriter(logFile, true)
			  try {
				  
				  fw.write(line)
			  }
			  finally fw.close() 
	    }
	  }
	

	
	def logFileResults(file:String, rs:ResultsStats, timeLimit : Int = TIME_LIMIT) = {
	  val fullPathFile = "data/bacp/instances12/" + file
	  val results = Array(solve(fullPathFile,SHAW,timeLimit), solve(fullPathFile,CURRENT,timeLimit), solve(fullPathFile,EXTENDED,timeLimit))
	  rs.addResult(results)
	  val line:String = file + "\t" + timeLimit + "\t" + results.mkString("\t") + "\n" 
	  log(line)
	}
	
	def solve(file : String, method : Int, timeLimit : Int) = {
 
	    val lines = Source.fromFile(file).getLines.reduceLeft(_ + " " + _)
	    val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
	    var index = 0
	    def next() = {
	      index += 1
	      vals(index - 1)
	    }
	
	    val nbCourses = next()
	    val courses = 0 until nbCourses
	    val nbPeriods = next()
	    val periods = 0 until nbPeriods
	    val mincredit = next()
	    val maxcredit = next()
	    val nbPre = next()
	    val credits = Array.fill(nbCourses)(next())
	    val prerequisites = Array.fill(nbPre)((next(), next()))
	
	    val cp = CPSolver()
	    cp.timeLimit = timeLimit
	    var x = Array.fill(nbCourses)(CPVarInt(cp,periods))
	    val l = Array.fill(nbPeriods)(CPVarInt(cp,0 to credits.sum))
	    val c = Array.fill(nbPeriods)(CPVarInt(cp,5 to 7))
	    //val c = Array.fill(nbPeriods)(CPVarInt(cp,0 to nbCourses))
	    val vari = CPVarInt(cp,0 to 10000000)
	
	
	
	    cp.minimize(vari) subjectTo {
	    //cp.minimize(maximum(l)) subjectTo {
	      
	        cp.add(spread(l,credits.sum, vari))
	        cp.add(binPacking(x, credits, l),Strong)
	        cp.add(gcc(x,0 until nbPeriods,5,7))
	        
	        method match 
	        {
	          case CURRENT => cp.add(new BinPackingFlow(x, credits, l, c))
	          case EXTENDED => cp.add(new BinPackingFlowExtended(x, credits, l, c))
	          case _ => Unit
	        }
	       
	        for ((i,j) <- prerequisites) {
	          cp.add(x(i) < x(j)) // precedence constraint
	        } 
	    } exploration {
	    	cp.deterministicBinaryFirstFail(x,x => selectMin(periods)(x.hasValue(_))(l(_).min).get)
	        //cp.binaryFirstFail(x,x => selectMin(periods)(x.hasValue(_))(l(_).min).get)
	        println(c.mkString(","))
	    } run()
	    
	    //cp.printStats
	    (cp.objective.isOptimum,cp.objective.objs(0).best,cp.bkts,cp.time)
	}

}


class ResultsStats {
  var results = Array[(Int,Int,Int,Long)]()
  var nbResults = 0
  
  def numericTuple(r : (Boolean,Int,Int,Long)) = (if(r._1) 1 else 0, r._2, r._3, r._4)
  
  def addResult(result : Array[(Boolean,Int,Int,Long)])
  {
    
    if (nbResults == 0)
    {
      results = result.map(numericTuple(_))
    }
    else
    {
	    for ((t:(Int,Int,Int,Long), i:Int) <- result.zipWithIndex)
	    {
	      val r = numericTuple(t)
	      results(i) = (results(i)._1 + r._1,results(i)._2 + r._2,results(i)._3 + r._3,results(i)._4 + r._4)
	      
	    }
    }
      
    nbResults += 1
  }
  
}
