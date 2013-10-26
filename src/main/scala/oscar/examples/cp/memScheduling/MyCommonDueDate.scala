package oscar.examples.cp.memScheduling

import oscar.cp.modeling._
import oscar.cp.memScheduling._
import oscar.cp.memScheduling.instances._
import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import scala.io.Source
import oscar.cp.constraints.SweepMaxCumulative

object MyCommonDueDate extends App with Scheduler with Reader {
  
  // Extending App yields a strange bug of Scala. We are overriding main until it is fixed.
    
  // Parsing
	  read fromFile("data/memScheduling/common-due-date/sch1000/cdd1000_8.txt")
	  val Array(nbJobs) = read fileFor 3 int
	  val Array(jobs, processingTimes, earliness, tardiness) = read fileFor nbJobs unitsOf 3 int
	  val h = 0.8		// External parameter to set a more or less restrictive due date, to be set by user.
	  val dueDate = (processingTimes.sum * h) toInt
	  
	  //Modeling
	  horizon setTo processingTimes.sum // Since there is only one machine, the horizon will be the sum of all duration
	  
	  //val activities = Activities(processingTimes)
	  val durationsVar = Array.tabulate(nbJobs)(t => CPVarInt(cp,processingTimes(t)))
	  val startsVar = Array.tabulate(nbJobs)(t => CPVarInt(cp, 0 to horizon - durationsVar(t).min))
	  val endsVar = Array.tabulate(nbJobs)(t => CPVarInt(cp, durationsVar(t).min to horizon))
	  val earlypen = Array.tabulate(nbJobs)(t => CPVarInt(cp, 0 to horizon*earliness(t)))
	  val tardipen = Array.tabulate(nbJobs)(t => CPVarInt(cp, 0 to horizon*tardiness(t)))
	  val max = maximum(earlypen)
	  val min = maximum(tardipen)
	  val makespan = max+min

	  // Creer une val calculant le earliness et le tardiness de chaque jobs par rapport � l'heure temps assign�
	  // Additionner les penalites
	  // Puis minimiser ce makespan durant la recherche.
	  
	  // Constraints & Search
	  
	  cp.minimize(makespan) subjectTo {
	    
	    // Consistency 
		  for (t <- Array(nbJobs)) {
			  cp.add(endsVar(t) == startsVar(t) + durationsVar(t))
		  }
		  // Precedences
		  for (t <- 1 to Array(nbJobs).max if jobs(t - 1) == jobs(t)) {
			  cp.add(endsVar(t - 1) <= startsVar(t))
		  }
		  
		  // !!!! I added .getMax to endsVar to get the maximum possible value for the CPVarInt endsVar(t).
		  // THIS MAY NOT ACHIEVE WHAT YOU WANTED! I just wanted to get rid of the errors, so beware!
		  for (t <- Array(nbJobs)) {
		    if (endsVar(t).getMax < dueDate) {
		      cp.add(earlypen(t) == earliness(t)*(dueDate - endsVar(t).getMax))
		    }
		    else if (dueDate < endsVar(t).getMax){
		      cp.add(tardipen(t) == tardiness(t)*(endsVar(t).getMax - dueDate))
		    }
		  }
	  } exploration {
		  cp binaryFirstFail startsVar
	  }
	  
	  cp run
	  
	  
  // Testing parsed values
  println(nbJobs)
  for (j <- jobs) {
    print(jobs(j) + " " + processingTimes(j) + " " + earliness(j) + " " + tardiness(j))
    println
  }
  println(dueDate)
  
  // TODO: Well... you know what to do, right? :)
  
}