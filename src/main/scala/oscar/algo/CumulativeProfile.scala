/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.algo

import oscar.cp.scheduling._

object CumulativeProfile {
  
	/** This algorithm returns the list of points representing the profile of an array of activities. 
	 * 
	 */
	def getCumulativeProfile(activities : Array[CumulativeActivity]) = {
		
		var points = new Array[Tuple2[Int, Int]](activities.size*2)
		
		// Add the EST and LCT of each activity with its influence on the global consumption. 
		for (i <- 0 until activities.size) {
			
			points(i*2) = (activities(i).est, activities(i).maxHeight) 
			points(i*2+1) = (activities(i).lct, -activities(i).maxHeight)
		}
		
		// Sort the points by time
		val sortedPoints = points.sortBy(_._1)
		
		var lastPoint = (0,0)
		
		var profilePoints : List[Tuple2[Int, Int]] = lastPoint :: Nil
		
		for (i <- 0 until sortedPoints.size) {
			
			if (lastPoint._1 == sortedPoints(i)._1) {
				
				lastPoint = (sortedPoints(i)._1, lastPoint._2 + sortedPoints(i)._2)	
				
			} else {

				profilePoints = lastPoint :: profilePoints
				profilePoints = (sortedPoints(i)._1, lastPoint._2) :: profilePoints
				
				lastPoint = (sortedPoints(i)._1, lastPoint._2 + sortedPoints(i)._2)				
			}
		}
		
		(lastPoint :: profilePoints).toArray.reverse
	}
}
