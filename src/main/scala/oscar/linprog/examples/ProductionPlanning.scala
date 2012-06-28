/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package oscar.linprog.examples


import oscar.linprog.modeling._


/**
 * A number of 12 products can be produced, each of which has a set of features, such as volume, weight, etc. 
 * There is a capacity constraint on the total amount that can be produced from each feature; 
 * for example, an upper limit of the total weight of the produced products. 
 * Also, each product generates a profit per unit produced. 
 * The objective is to maximize the total profit, while satisfying capacity constraints.
 * @author pschaus@gmail.com
 */
object ProductionPlanning  extends LPModel {
	
	def main(args: Array[String]) {
		
		val b = Array(18209, 7692, 1333, 924, 26638, 61188, 13360 ) // Dimensions
		val c = Array(96, 76, 56, 11, 86, 10, 66, 86, 83, 12, 9, 81) // Products
		
		val Dimensions = 0 until b.size
		val Products = 0 until c.size
		
		val coef = Array(Array(19,   1,  10,  1,   1,  14, 152, 11,  1,  1, 1, 1), //Dimensions x Products
						 Array( 0,   4,  53,  0,   0,  80,   0,  4,  5,   0, 0, 0),
						 Array( 4, 660,   3,  0,  30,   0,   3,  0,  4,  90, 0, 0),
						 Array( 7,   0,  18,  6, 770, 330,   7,  0,  0,   6, 0, 0),
						 Array( 0,  20,   0,  4,  52,   3,   0,  0,  0,   5, 4, 0),
						 Array( 0,   0,  40, 70,   4,  63,   0,  0, 60,   0, 4, 0),
						 Array( 0,  32,   0,  0,   0,   5,   0,  3,  0, 660, 0, 9))


		val lp = LPSolver()
		val x = Products.map(p => LPVar(lp,"x",0,10000))

		lp.maximize(sum(Products){p => x(p)*c(p)}) subjectTo {
			for (d <- Dimensions) {
				lp.add(sum(Products) (p => coef(d)(p) * x(p)) <= b(d))
			}	
		}
		
		for (d <- Dimensions) {
				lp.add(sum(Products) (p => coef(d)(p) * x(p)) <= b(d))
		}
		
		println("objective: "+lp.getObjectiveValue)
		Products.foreach(p => if (x(p).getValue > 0) println("x"+p+":"+x(p).getValue))
		
		lp.release()

	}

}
