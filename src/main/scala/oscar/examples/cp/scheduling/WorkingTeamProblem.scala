package oscar.examples.cp.scheduling

import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.scheduling._
import oscar.search._
import oscar.visual._

import scala.util.Random.nextInt

import scala.collection.mutable.Set

object WorkingTeamProblem {
  
	def main(args : Array[String]) {
	  
		// Data	
		// -----------------------------------------------------------------------

		val nTasks = 5
		val nTeams = 3
		
		val Tasks = 0 until nTasks
		val Teams = 0 until nTeams
		
		val tasksDurations  : Array[Int] = Array.fill(nTasks)(nextInt(20)+3)
		
		val teamDur   : Array[Int] = Array.fill(nTeams)(nextInt(3)+1)
		val teamPower : Array[Int] = Array.fill(nTeams)(nextInt(4)+1)
		val teamCost  : Array[Int] = Array.fill(nTeams)(nextInt(21)+5)
		
		// Upper bound of the horizon
		val horizon = tasksDurations.sum

		// Modeling	
		// -----------------------------------------------------------------------
  	   	
  	   	val cp = CPSolver()

		// Matrix of cumulative activities (each line represents a job)
		val tasks = Array.tabulate(nTasks)(i => {
			
  	   		val dur      = CPVarInt(cp, tasksDurations(i))
  	   		val start    = CPVarInt(cp, 0 to horizon - dur.getMin)
  	   		
  	   		CumulativeActivity(start, dur, 0, 1)
  	   	}) 	
  	   	
  	   	val teamCoeff : Array[CPVarInt] = Array.tabulate(nTeams)(i => CPVarInt(cp, 0 to horizon/teamDur(i)+1))
  	   	
  	   	val teams = Array.tabulate(nTeams)(i => {
			
  	   		val dur      = CPVarInt(cp, 0 to (horizon/teamDur(i)+1)*teamDur(i))
  	   		val start    = CPVarInt(cp, 0 to horizon - dur.getMin)
  	   		
  	   		CumulativeActivity(start, dur, 0, -teamPower(i))
  	   	}) 	

  	   	// The make span to minimize
  	   	val cost = sum(Teams)(i => teamCoeff(i)*teamCost(i))
  	   	
  	   	val allAct = tasks++teams
  	   	
  	   	// Visualization  
  	   	// -----------------------------------------------------------------------
  	   		
  	   	val frame = new VisualFrame("Cumulative Job-Shop Problem", 2, 1)
		
		val cols = VisualUtil.getRandomColorArray(1)
		val visualActivities = allAct.map(a => VisualActivity(a))
		
		// Profiles 
		val profile = new VisualProfile(visualActivities, 0, 0, cols(0))
		frame.createFrame("Profile").add(profile)

		frame.pack
  	   	
  	   	// Constraints and Solving
		// -----------------------------------------------------------------------
  	   	
  	   	cp.minimize(cost) subjectTo {
			
			// Precedence constraints
			for (i <- Teams)
				cp.add(teams(i).dur == teamCoeff(i)*teamDur(i))
			
			// Cumulative constraints
			cp.add(new MaxSweepCumulative(cp, allAct, 0, 0))

		} exploration {
			
			// Efficient but not complete search strategy
			
			SchedulingUtils.setTimesSearch(cp, allAct)
			cp.binaryFirstFail(teamCoeff)
			
			// Updates the visual components
			profile.update(20, 20)
		}    
		
		cp.printStats() 
	}
}
