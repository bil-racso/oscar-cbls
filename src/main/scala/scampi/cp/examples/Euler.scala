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
import scampi.cp.core._
import scampi.cp.search._


/**
 * 
 * Euler Problem, a knight must visit every position of a chess board once and come back to its initial position
 * using only valid knight moves.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object Euler  extends CPModel {
	def main(args: Array[String]) {

		def reachables(i : Int) : Set[Int] = {
			def onBoard (y : Int*) : Set[Int] = y.filter(x=> x>=0 && x<=63).toSet
			i%8 match {
				case 0 =>  onBoard(i-15,i-16,i+10,i+17)
				case 1 =>  onBoard(i-17,i-15,i-6,i+10,i+15,i+17)
				case 6 =>  onBoard(i-17,i-15,i-10,i+6,i+15,i+17)
				case 7 =>  onBoard(i-17,i-10,i+6,i+15)
				case _ =>  onBoard(i-17,i-15,i-10,i-6,i+6,i+10,i+15,i+17)
			}		
		}
		
		val cp = new CPSolver()
		val x = (0 until 64).map(v => CPVarInt(cp,reachables(v)))
		
		cp.solve subjectTo {
			cp.add(circuit(x))
		} exploration {
		  cp.binaryFirstFail(x)
		  println(x.map(_.getValue).mkString(","))
		}
		
		cp.printStats()


	}

}