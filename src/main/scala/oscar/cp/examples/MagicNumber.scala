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
import oscar.search._



/**
 * Create a number using only the digits 4,4,3,3,2,2,1 and 1. 
 * So it can only be eight digits. 
 * You have to make sure the ones are separated by one digit, 
 *                     the twos are separated by two digits 
 *                     the threes are separated with three digits and 
 *                     the fours are separated by four digits
 * @author Pierre Schaus pschaus@gmail.com
 */
object MagicNumber  extends CPModel {
	def main(args: Array[String]) {
		
      val cp = CPSolver()
      
      var one = CPVarInt(cp,0 to 7)
      var two = CPVarInt(cp,0 to 7)
      var three = CPVarInt(cp,0 to 7)
      var four = CPVarInt(cp,0 to 7)
      var x = Array.fill(8)(CPVarInt(cp,1 to 4))
      
      cp.solveAll subjectTo {
    	  cp.add(element(x,one,1))
    	  cp.add(element(x,one+2,1))
    	  cp.add(element(x,two,2))
    	  cp.add(element(x,two+3,2))
    	  cp.add(element(x,three,3))
    	  cp.add(element(x,three+4,3))
    	  cp.add(element(x,four,4))
    	  cp.add(element(x,four+5,4))
    	  cp.add(gcc(x,1 to 4,2,2))
    	  cp.add(one < two)
      } exploration {        
        cp.binaryFirstFail(Array(one,two,three,four))
        println(x.mkString((",")))
      }
      cp.printStats() 
	}
}
