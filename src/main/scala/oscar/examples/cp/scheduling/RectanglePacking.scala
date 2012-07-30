/*******************************************************************************
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
 ******************************************************************************/

package oscar.examples.cp.scheduling

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.search._
import oscar.visual._
import scala.math
import java.awt.Color
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.constraints.MaxSweepCumulative

/**The problem is to pack rectangles in a larger rectangles without
 * exceeding its bounds.
 * 
 * This instance contains 24 rectangles
 * 
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object RectanglePacking {

	def main(args: Array[String]) {
	    
		val x = 8
	    val y = 12
	    
	    val ySide  = Array(2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 3, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2)
	    val xSide  = Array(4, 4, 2, 2, 1, 5, 2, 2, 3, 3, 3, 4, 4, 2, 2, 1, 5, 2, 2, 3, 3, 3, 5)
	    
	    val nRectangles = xSide.size
		val Rectangles  = 0 until nRectangles
	    
	    val cp = CPSolver()
	    
	    val xPosition = Array.tabulate(nRectangles)(i => CPVarInt(cp,0 until x))
	    val yPosition = Array.tabulate(nRectangles)(i => CPVarInt(cp,0 until y))
	    
	    val width  = Array.tabulate(nRectangles)(i => CPVarInt(cp, ySide(i) to xSide(i)))
	    val height = Array.tabulate(nRectangles)(i => CPVarInt(cp, ySide(i) to xSide(i)))
	    
	    val o = Array.tabulate(nRectangles)(i => CPVarBool(cp))
	
	    var solFound = false
	    
	    cp.solve subjectTo {
	    	
	    	for (i <- Rectangles; j <- i+1 until nRectangles) {
	    		
	    		cp.add((xPosition(i) + width(i)  <== xPosition(j)) ||
	    	           (xPosition(j) + width(j)  <== xPosition(i)) ||
	    	           (yPosition(i) + height(i) <== yPosition(j)) ||
	    	           (yPosition(j) + height(j) <== yPosition(i)))
	    	}
	    	
	    	for (i <- Rectangles) {
	    		cp.add(xPosition(i) + width(i) <= x)
	    		cp.add(yPosition(i) + height(i) <= y)
	    	}
	    			
	    	for (i <- Rectangles) {
	    		cp.add(width(i)  == (!o(i))*ySide(i) + o(i)*xSide(i))
	    		cp.add(height(i) == (!o(i))*xSide(i) + o(i)*ySide(i))
	    	}
	    	
	    	val activitiesx = Array.tabulate(nRectangles)(i => new CumulativeActivity(xPosition(i), 
	    																			  width(i), 
	    																			  xPosition(i) + width(i), 
	    																			  CPVarInt(cp,0), 
	    																			  height(i)))     
	    	cp.add(new MaxSweepCumulative(cp, activitiesx, y, 0))
	      
	    	val activitiesy = Array.tabulate(nRectangles)(i => new CumulativeActivity(yPosition(i), 
	    																			  height(i), 
	    																			  yPosition(i) + height(i), 
	    																			  CPVarInt(cp,0), 
	    																			  width(i))) 
	    	cp.add(new MaxSweepCumulative(cp, activitiesy, x, 0))
	
		} exploration {

			val perm = (0 until nRectangles).sortBy(i => -xSide(i)*ySide(i))
			val xpos = perm.map(xPosition(_))
			val ypos = perm.map(yPosition(_))
			cp.binary(xpos)
			cp.binary(ypos)
			
			solFound = true
		}
			
		cp.printStats()
		
	    // Visualization
	    if (solFound) {
	    	
	    	val f = new VisualFrame("Rectangle Packing") 
	    	val ff = f.createFrame("Packing")
	    	val d = new VisualDrawing(false,false)
	    	
	    	ff.add(d)
	    	
	    	def scale = 50
	    	
	    	val bg = new VisualRectangle(d,0,0,x*scale,y*scale)
	    	bg.setInnerCol(Color.black)
	    	
	    	(Rectangles).foreach { i =>
	    		val r = new VisualRectangle(d,
	    									xPosition(i).value*scale,
	    									yPosition(i).value*scale,
	    									width(i).getValue*scale,
	    									height(i).getValue*scale)
	    		
	    		r.setInnerCol(VisualUtil.getRandomColor())
	    	}
	    }
	}
}
