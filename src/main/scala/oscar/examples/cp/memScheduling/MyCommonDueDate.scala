package oscar.examples.cp.memScheduling

import oscar.cp.modeling._
import oscar.cp.memScheduling._
import oscar.cp.memScheduling.instances._
import oscar.visual._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import scala.io.Source
import oscar.cp.constraints.SweepMaxCumulative

object MyCommonDueDate extends App with Scheduler with Reader {
  
  // Extending App yields a strange bug of Scala. We are overriding main until it is fixed.
    
	// Parsing
	  read fromFile("data/memScheduling/common-due-date/sch10/cdd10_1.txt")
	  val Array(nbJobs) = read fileFor 1 int
	  val Array(jobs, processingTimes, earliness, tardiness) = read fileFor nbJobs unitsOf 3 int
	  val h = 0.8		// External parameter to set a more or less restrictive due date, to be set by user.
	  val dueDate = (processingTimes.sum * h) toInt
	  
	  // Testing parsed values
	  println(nbJobs)
	  for (j <- jobs) {
		  print(jobs(j) + " " + processingTimes(j) + " " + earliness(j) + " " + tardiness(j))
		  println
	  }
	  println(dueDate)
	  
	  //Modeling
	  horizon setTo processingTimes.sum // Since there is only one machine, the horizon will be the sum of all duration
	  
	  // Je pense que ça devrait marcher ça maintenant! Pour remplacer toutes tes lignes là... :p
	  //val activities = Activities(processingTimes, earliness, tardiness)
	  //activities needs Array(3) ofResources Array(UnitResource())
	  //val test1: Array[Int] = activities earlinessPenalty
	  //val test2: Array[CPVarInt] = activities dur 	// Je renomerai peut être bien cette fonction "duration" plutot... 
	  val durationsVar = Array.tabulate(nbJobs)(t => CPVarInt(cp,processingTimes(t)))
	  val startsVar = Array.tabulate(nbJobs)(t => CPVarInt(cp, 0 to horizon - durationsVar(t).min))
	  val endsVar = Array.tabulate(nbJobs)(t => CPVarInt(cp, durationsVar(t).min to horizon))
	  val earlypen = Array.tabulate(nbJobs)(t => CPVarInt(cp, 0 to horizon*earliness(t)))	  
	  val tardipen = Array.tabulate(nbJobs)(t => CPVarInt(cp, 0 to horizon*tardiness(t)))
	  var temp = 0
	  for (j <- jobs){
	    if (earlypen(j).getMax > tardipen(j).getMax){
	      temp = temp + earlypen(j).getMax
	    }
	    else temp = temp + tardipen(j).getMax
	  }
	  val makespan = CPVarInt(cp, 0 to temp)
	  println(makespan)

	  // Creer une val calculant le earliness et le tardiness de chaque jobs par rapport à l'heure temps assigné (ce serait pas "leur" plutot?)
	  // Additionner les penalites
	  // Puis minimiser ce makespan durant la recherche.
	  
	  // Constraints & Search
	  
	  cp.minimize(makespan) subjectTo {
	    
	    // Consistency 
		  for (t <- 0 until nbJobs-1) {
			  cp.add(endsVar(t) == startsVar(t) + durationsVar(t))
		  }
		  // Precedences
		  for (t <- 1 until nbJobs-1 if jobs(t - 1) == jobs(t)) {
			  cp.add(endsVar(t - 1) <= startsVar(t))
		  }
		  //Penalty calcul
		  // Fonctionne pas ... Assistant avait dit qu'on aurait besoin que de sweep max cumulative.
		  // + makespan = sum earlypen + tardipen.
		  for (t <- 0 until nbJobs-1) {
		    if (endsVar(t).getValue < dueDate) {
		      cp.add(earlypen(t) == earliness(t)*(dueDate - endsVar(t).getValue))
		    }
		    else if (dueDate < endsVar(t).getValue){
		      cp.add(tardipen(t) == tardiness(t)*(endsVar(t).getValue - dueDate))
		    }
		  }
	  } exploration {
		  cp binaryFirstFail earlypen
	  }
	  
	  cp run
	  
	  cp printStats
	 
  
}