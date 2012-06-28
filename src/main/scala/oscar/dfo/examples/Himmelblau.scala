/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package oscar.dfo.examples

import oscar.dfo.modeling._

/**
 * @author pierre.schaus@n-side.com
 */
object Himmelblau extends DFOModel{

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
