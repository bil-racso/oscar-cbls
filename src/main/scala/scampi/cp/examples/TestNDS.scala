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
import scala.util.continuations._
import scampi.reversible.ReversibleBool



/**
 * 
 * Euler Problem, a knight must visit every position of a chess board once and come back to its initial position
 * using only valid knight moves.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object TestNDS  extends CPModel {
  def main(args: Array[String])  {

    
    def loopWhile[T](cond: =>Boolean)(body: =>(Unit @suspendable)): Unit @suspendable = {
     if (cond) {
       body
       loopWhile[T](cond)(body)
     } 
    }
    
    def loop(r: Range)(body: Int =>(Unit @suspendable)): Unit @suspendable = {
      var i = r.start
      loopWhile(i < r.end) {
        val k = i
        body(i)
        i = k+1
      }
    }
      
		
	val cp = CPSolver()
	val v = Array.tabulate(9)(i => new ReversibleBool(cp))
	var nb = 0
	cp.exploration {		
	     loop(0 until v.length) {
    	     i => cp.branch {//println("left "+i)
    	                       v(i).setValue(true)} 
    	                      {//println("right "+i)
    	                       v(i).setValue(false)}
         }
	     nb += 1
	     println("solution:"+v.mkString(","))
	}
	println("nb:"+nb)



  }

}