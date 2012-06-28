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
