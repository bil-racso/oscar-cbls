package oscar.examples.cp

import oscar.cp.constraints.MultiCumulative
import oscar.cp.constraints.Cumulative
import oscar.cp.constraints.NaiveMultiCumulative
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.cp.scheduling._
import oscar.visual._

object SmallCumulativeProblem extends CPModel {
  
	def main(args: Array[String]) {
		
		// Modeling
		val cp = CPSolver()
		
		val r1 = new CumulativeActivity(new CPVarInt(cp, 0 to 70),  // start
										new CPVarInt(cp, 30 to 30),  // duration
										new CPVarInt(cp, 0 to 100), // end
										new CPVarInt(cp, 0 to 0),  // machine
										new CPVarInt(cp, 1 to 1))  // resource
		
		val r2 = new CumulativeActivity(new CPVarInt(cp, 0 to 80),  // start
										new CPVarInt(cp, 20 to 20),  // duration
										new CPVarInt(cp, 0 to 100), // end
										new CPVarInt(cp, 0 to 0),  // machine
										new CPVarInt(cp, 2 to 2))  // resource
		
		val r3 = new CumulativeActivity(new CPVarInt(cp, 0 to 50),  // start
										new CPVarInt(cp, 50 to 50),  // duration
										new CPVarInt(cp, 0 to 100), // end
										new CPVarInt(cp, 0 to 0),  // machine
										new CPVarInt(cp, 2 to 2))  // resource
		
		val r4 = new CumulativeActivity(new CPVarInt(cp, 0 to 50),  // start
										new CPVarInt(cp, 50 to 50),  // duration
										new CPVarInt(cp, 0 to 100), // end
										new CPVarInt(cp, 0 to 0),  // machine
										new CPVarInt(cp, 3 to 3))  // resource
		
		val r5 = new CumulativeActivity(new CPVarInt(cp, 0 to 0),  // start
										new CPVarInt(cp, 100 to 100),// duration
										new CPVarInt(cp, 100 to 100),// end
										new CPVarInt(cp, 0 to 0),  // machine
										new CPVarInt(cp, -1 to -1))// resource
		
		val rectangles = Array(r1, r2, r3, r4, r5)
		val capacities = Array(2)
		
		// -----------------------------------------------------------------------
  	   	
  	   	// Visualization   	
  	   	val frame = new VisualFrame("Small Cumulative Problem", 1, 1)
		
		val drawing = new VisualDrawing(false)
		
		val yScale = 50
		val xScale = 5
		
		val cols = VisualUtil.getRandomColorArray(rectangles.size)
		
		val visualActivities: Set[VisualActivity] = rectangles.map(v => VisualActivity(v)).toSet

		val profile = new VisualProfile(visualActivities, cols(0))
		profile.capacity = 2		
		frame.createFrame("Profile").add(profile)
		profile.draw(xScale, yScale)
	   
		def updateVisu() = { profile.update(xScale, yScale)	}
	   
		// -----------------------------------------------------------------------
		
	  
  	   	// Constraints and Solving
  	   	cp.solve subjectTo {

			for (i <- 0 until rectangles.size) 
				cp.add(rectangles(i).getStart + rectangles(i).getDur == rectangles(i).getEnd)
			
			//cp.add(new MultiCumulative(cp, rectangles, capacities, true))
			//NaiveMultiCumulative.multiCumulative(cp, rectangles, capacities)
			cp.add(new Cumulative(cp, rectangles, 2, 2, 0))
			
	   } exploration {
	       
		   cp.binaryFirstFail(rectangles.map(_.getStart))
		   updateVisu
	   }
       cp.printStats()
       println(r1.getStart)
       println(r2.getStart)
       println(r3.getStart)
       println(r4.getStart)
       println(r5.getStart)
	}
}
