/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.examples


import oscar.cp.modeling._
import oscar.cp.search._

/**
 * Martin Gardner Problem:
 * Call a number "prime-looking" if it is composite but not divisible by 2,3 or 5.
 * The three smallest prime-looking numbers are 49, 77 and 91. 
 * There are 168 prime numbers less than 1000. 
 * How many prime-looking numbers are there less than 1000?
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object PrimeLooking extends CPModel {
  def main(args: Array[String]) {
    
    val n = 1000
    
    val cp = new CPSolver()
    val x = CPVarInt(cp,1 to n)
    val d1 = CPVarInt(cp,2 to n)
    val d2 = CPVarInt(cp,2 to n)
    
    var cpt = 0 // number of solution
    
    cp.solveAll subjectTo {
      cp.add(d1*d2 == x)
      cp.add(d1 <= d2) // avoid symmetric solutions
      // prevent divisibility by 2,3 and 5
      for (v <- List(2,3,5)) {
        for (i <- 1 to 1000 if (v*i <= n)) {
          cp.add(d1 != (v*i))
    	  cp.add(d2 != (v*i))
        }
      }
    } exploration {
      cp.binaryFirstFail(Array(x,d1,d2))
      cpt += 1
    }
    
    println("number of solutions:"+cpt)
  }
  


}
