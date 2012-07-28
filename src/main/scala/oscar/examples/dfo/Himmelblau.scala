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

package oscar.examples.dfo

import oscar.dfo.modeling._
import oscar.dfo._
import oscar.algebra._

/**
 * @author pschaus@gmail.com
 */
object Himmelblau {

  def main(args: Array[String]): Unit = {  
    
      val dfo = DFOSolver()
      

      // declare two variables and their domain
      val x = DFOVar(dfo, "x1", -4, +4)
      val y = DFOVar(dfo, "x2", -4, +4)
    
      // Himmelblau function
      // 4 local minima: 
      //  f(-0.27,-0.92) = 181.61
      //  f(-3,2) = 0
      //  f(-2.8,-3.13) = 0
      //  f(-3.77,-3.28) = 0
      // f(-3.58,-1.84) = 0
      val objective = (x*x + y - 11) * (x*x + y - 11) + (x + y*y - 7) * (x + y*y - 7)
     
      // callback to print evolution of objective during optimization
      dfo.onSolution {
       println(objective.value)
      }    
      
      // start the effective optimization
      dfo.minimize(objective)
      
      println(x+" "+x.value)
      println(y+" "+y.value)
      println("objective:"+objective.value) 
  
  }

}
