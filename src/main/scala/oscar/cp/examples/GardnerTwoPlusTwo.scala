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
 * 
 * In this addition problem, each letter stands for a different digit.
 * 
 *   T W O
 * + T W O
 * --------
 * F O U R
 *
 * If T = 7 and the letter O represents an even number, what is the only possible value for W
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object GardnerTwoPlusTwo  extends CPModel {

	def main(args: Array[String]) {

	  val cp = CPSolver()
	  val T = CPVarInt(cp,0 to 9)
	  val W = CPVarInt(cp,0 to 9)
	  val O = CPVarInt(cp,Set(0,2,4,6,8)) // even number
	  val F = CPVarInt(cp,0 to 9)
	  val U = CPVarInt(cp,0 to 9)
	  val R = CPVarInt(cp,0 to 9)
	  
	  cp.solveAll() subjectTo {
	    cp.add((T*100+W*10+O)*2 == F*1000+O*100+U+10+R)
	    cp.add(alldifferent(Array(T,W,O,F,U,R)), Strong)
	  } exploration {
	    cp.binaryFirstFail(Array(T,W,O,F,U,R))
	    println("T:"+T+" W:"+W+" O:"+O+" F:"+F+" U:"+U+" R:"+R)
	  }
	  
	  cp.printStats()
		

	}

	

	

}
