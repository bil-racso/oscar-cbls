/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.examples


import scampi.cp.modeling._
import scampi.search._


/**
 * A number with an interesting property:
 *
 * When I divide it by  2, the remainder is 1.
 * When I divide it by  3, the remainder is 2.
 * When I divide it by  4, the remainder is 3.
 * When I divide it by  5, the remainder is 4.
 * When I divide it by  6, the remainder is 5.
 * When I divide it by  7, the remainder is 6.
 * When I divide it by  8, the remainder is 7.
 * When I divide it by  9, the remainder is 8.
 * When I divide it by 10, the remainder is 9.
 * 
 * It's not a small number, but it's not really big, either.
 * When I looked for a smaller number with this property I couldn't find one.
 * Can you find it?
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object MagicModuloNumber  extends CPModel {
	def main(args: Array[String]) {
		
      val cp = CPSolver()
      
      var x = CPVarInt(cp,0 to 10000)
      
      
      cp.solveAll subjectTo {
    	  for (i <- 2 to 10) {
    	    cp.add(modulo(x,i,i-1))
    	  }
      } exploration {        
        cp.binaryFirstFail(Array(x))
        println(x)
      }
      cp.printStats() 
	}
}