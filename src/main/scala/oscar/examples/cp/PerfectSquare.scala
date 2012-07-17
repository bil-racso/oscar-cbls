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

package oscar.examples.cp


import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.search._
import oscar.visual._
import scala.math
import java.awt.Color
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.constraints.BoundedCumulative
import oscar.cp.constraints.MaxCumulative
import oscar.cp.constraints.MinCumulative

/**
 *
 * The problem is to fully cover a big 112 square with 
 * 21 different smaller squares with no overlap between squares.
 * @author Pierre Schaus pschaus@gmail.com
 */
object PerfectSquare extends CPModel {

  def main(args: Array[String]) {
    val s = 112
    val side = Array(50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2)
    val cp = CPSolver()
    val x = Array.tabulate(side.size)(i => CPVarInt(cp,0 to s-side(i)))
    val y = Array.tabulate(side.size)(i => CPVarInt(cp,0 to s-side(i)))

    var solFound = false
    
    cp.solve subjectTo {
      for (i <- 0 until x.size; j <- i+1 until x.size) {
    	  cp.post((x(i) + side(i) <== x(j)) ||
    	          (x(j) + side(j) <== x(i)) ||
    	          (y(i) + side(i) <== y(j)) ||
    	          (y(j) + side(j) <== y(i)))
      }
      
      /*
      // decomposition of redundant constraint
      def overlap(vars: Array[CPVarInt], i: Int, p: Int) = (vars(i) <== p) && (vars(i)+side(i) >>= p)
      for(p <- 0 until s) {
        cp.post(sum(0 until x.size) (i => overlap(x,i,p) * side(i)) == s)
        cp.post(sum(0 until x.size) (i => overlap(y,i,p) * side(i)) == s)
      }*/
      
      val activitiesx = Array.tabulate(x.size)(i => new CumulativeActivity(x(i), CPVarInt(cp,side(i)), x(i)+side(i), CPVarInt(cp,0), CPVarInt(cp,side(i))))     
      cp.post(new BoundedCumulative(cp,activitiesx,s,s,0))
      //cp.post(new MaxCumulative(cp, activitiesx, s, 0))
      //cp.post(new MinCumulative(cp, activitiesx, s, 0))
      
      val activitiesy = Array.tabulate(x.size)(i => new CumulativeActivity(y(i), CPVarInt(cp,side(i)), y(i)+side(i), CPVarInt(cp,0), CPVarInt(cp,side(i))))     
      cp.post(new BoundedCumulative(cp,activitiesy,s,s,0))
      //cp.post(new MaxCumulative(cp, activitiesy, s, 0))
      //cp.post(new MinCumulative(cp, activitiesy, s, 0))
      
    } exploration {
    	def label(w: Array[CPVarInt]) = {
    			while (!cp.allBounds(w)) {
    				// minimum x position 
    				val pos = w.filter(!_.isBound()).map(_.getMin()).min
    				val z = w.filter(w => !w.isBound() && w.hasValue(pos)).first
    				cp.branch(cp.post(z == pos))(cp.post(z != pos))
    			}
    	}
    	label(x)
    	label(y)
        solFound = true
    }
    cp.printStats()
    // ----------------- visu --------------------------------------------------
    if (solFound) {
    	val f = new VisualFrame("Pefect Square") 
    	val ff = f.createFrame("Square")
    	val d = new VisualDrawing(false,false)
    	ff.add(d)
    	def scale = 5
    	val bg = new VisualRectangle(d,0,0,s*scale,s*scale)
    	bg.setInnerCol(Color.black)
    	(0 until x.size).foreach { i =>
    	  val r = new VisualRectangle(d,x(i).getValue()*scale,y(i).getValue()*scale,side(i)*scale,side(i)*scale)
    	  r.setInnerCol(VisualUtil.getRandomColor())
    	}
    }
    // -------------------------------------------------------------------------
    
    
  }

}
