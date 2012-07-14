package oscar.examples.cp

import oscar.cp.constraints.MultiCumulative
import oscar.cp.constraints.MaxMultiCumulative
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
		
		val r1 = new CumulativeActivity(new CPVarInt(cp, 0 to 16),  // start
										new CPVarInt(cp, 1 to 1),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 2 to 2))   // resource
		
		val r2 = new CumulativeActivity(new CPVarInt(cp, 0 to 15),  // start
										new CPVarInt(cp, 2 to 2),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 1 to 1))   // resource
		
		val r3 = new CumulativeActivity(new CPVarInt(cp, 0 to 15),  // start
										new CPVarInt(cp, 2 to 2),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 1 to 1))   // resource
		
		val r4 = new CumulativeActivity(new CPVarInt(cp, 0 to 15),  // start
										new CPVarInt(cp, 2 to 2),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 2 to 2))   // resource
		
		val r5 = new CumulativeActivity(new CPVarInt(cp, 0 to 12),  // start
										new CPVarInt(cp, 5 to 5),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 1 to 1))   // resource
		
		val r6 = new CumulativeActivity(new CPVarInt(cp, 0 to 14),  // start
										new CPVarInt(cp, 3 to 3),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 3 to 3))   // resource
		
		val r7 = new CumulativeActivity(new CPVarInt(cp, 0 to 9),  // start
										new CPVarInt(cp, 8 to 8),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 1 to 1))   // resource
		
		val r8 = new CumulativeActivity(new CPVarInt(cp, 0 to 11),  // start
										new CPVarInt(cp, 6 to 6),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 1 to 1))   // resource
		
		val r9 = new CumulativeActivity(new CPVarInt(cp, 0 to 14),  // start
										new CPVarInt(cp, 3 to 3),   // duration
										new CPVarInt(cp, 0 to 17),  // end
										new CPVarInt(cp, 0 to 0),   // machine
										new CPVarInt(cp, 1 to 1))   // resource
		
		val r10 = new CumulativeActivity(new CPVarInt(cp, 0 to 14),  // start
										 new CPVarInt(cp, 3 to 3),   // duration
										 new CPVarInt(cp, 0 to 17),  // end
										 new CPVarInt(cp, 0 to 0),   // machine
										 new CPVarInt(cp, 1 to 1))   // resource
		
		val r11 = new CumulativeActivity(new CPVarInt(cp, 0 to 15),  // start
										 new CPVarInt(cp, 2 to 2),   // duration
										 new CPVarInt(cp, 0 to 17),  // end
										 new CPVarInt(cp, 0 to 0),   // machine
										 new CPVarInt(cp, 2 to 2))   // resource
		
		val r12 = new CumulativeActivity(new CPVarInt(cp, 0 to 16),  // start
										 new CPVarInt(cp, 1 to 1),   // duration
										 new CPVarInt(cp, 0 to 17),  // end
										 new CPVarInt(cp, 0 to 0),   // machine
										 new CPVarInt(cp, 3 to 3))   // resource
		
		
		
		
		val rectangles = Array(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
		val capacities = Array(2)
		
		// -----------------------------------------------------------------------
  	   	
  	   	// Visualization   	
  	   	val frame = new VisualFrame("Small Cumulative Problem", 1, 1)
		
		val drawing = new VisualDrawing(false)
		
		val yScale = 40
		val xScale = 40
		
		val cols = VisualUtil.getRandomColorArray(rectangles.size)
		
		val visualActivities = rectangles.map(v => VisualActivity(v))

		val profile = new VisualProfile(visualActivities, 0, 2, cols(0))
		profile.capacity = 2		
		frame.createFrame("Profile").add(profile)
	   
		def updateVisu() = { profile.update(xScale, yScale)	}
	   
		// -----------------------------------------------------------------------
		
		val starts = rectangles.map(_.getStart)
		val Rect = 0 to 12
	  
  	   	// Constraints and Solving
  	   	cp.solve subjectTo {

			for (i <- 0 until rectangles.size) 
				cp.add(rectangles(i).getStart + rectangles(i).getDur == rectangles(i).getEnd)
			
			//cp.add(new MultiCumulative(cp, rectangles, Array(2), true))
			cp.add(new MaxMultiCumulative(cp, rectangles, Array(3), true))
			//NaiveMultiCumulative.multiCumulative(cp, rectangles, capacities)
			//cp.add(new Cumulative(cp, rectangles, 2, 2, 0))
			
	   } exploration {
	       
		   cp.binaryFirstFail(rectangles.map(_.getStart))
		   
		   /*while (!allBounds(starts)) {
		  	   val unbounds = starts.filter(!_.isBound)
		  	   val maxDomSize = unbounds.map(_.getSize).max
		  	   val x = unbounds.filter(_.getSize == maxDomSize).first
		  	   val mid = (x.getMax - x.getMin) / 2
		  	   cp.branch(cp.post(x < mid))(cp.post(x >= mid))
		   }*/
		   updateVisu
	   }
	     
       cp.printStats()
       
       println(r1.getStart)
       println(r2.getStart)
       println(r3.getStart)
       println(r4.getStart)
       println(r5.getStart)
       println(r6.getStart)
       println(r7.getStart)
       println(r8.getStart)
       println(r9.getStart)
       println(r10.getStart)
       println(r11.getStart)
       println(r12.getStart)
	}
}
