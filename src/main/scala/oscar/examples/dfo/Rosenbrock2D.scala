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

/**
 * @author pierre.schaus@n-side.com
 */
object Rosenbrock2D extends DFOModel{

  def main(args: Array[String]): Unit = {  
    
      val dfo = DFOSolver()

      // declare two variables and their domain
      val x = DFOVar(dfo, "x1", -10, +10)
      val y = DFOVar(dfo, "x2", -10, +10)
    
      // 2D Rosenbrock function
      val objective = (1-x) * (1-x) + 100 * (y-x*x) * (y-x*x)
      
      // callback to print evolution of objective during optimization
      dfo.onSolution {
        () => println(objective.value)
      }    
      
      // start the effective optimization
      dfo.minimize(objective)

      println(x+" "+x.value)
      println(y+" "+y.value)
      println("objective:"+objective.value) 
  
  }

}
