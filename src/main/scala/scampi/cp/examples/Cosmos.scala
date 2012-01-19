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
import scala.io.Source
import scala.collection.mutable.Map
import scampi.search.Branching

/** 
 * Cosmos Game invented by Bertrand Cornelusse and Gilles Scouvart for the 10 years of n-Side:
 * Maximize the total market exchange such that demand and supply match at any time
 * @author Pierre Schaus pschaus@gmail.com
 */
object Cosmos extends CPModel {
	def main(args: Array[String]) {
	  
	  val cp = CPSolver()
	  
	  case class Order(data: Array[Int]) {
	    
	    val qty = data(0) // amount of electricity he is ready to produce (>0) or consume (<0)
	    val start = data(1) // [start,end] is the interval of validity of the order. 
	    val end = data(2)
	    val selected = CPVarBool(cp) // If the order is selected the orderer will have to produce/consume 
	                                 // the quantity at each period: start, start+1, ...., end-1, end.
	    def energy = qty.abs * (end - start + 1)
	    def overlap(t : Int) = t <= end && t >= start
	    var sol = true
	    def bound = selected.isBound()
	    
	    cp.onSolution {
	      sol = selected.isTrue()
	    }
	    def restore() =  if (sol) selected.constraintTrue() else selected.constraintFalse()
	  }
	  
	  val firstLine::restLines = Source.fromFile("data/cosmos.txt").getLines.toList
	  val n = firstLine.toInt
	  
	  val orders = restLines.map(_.split(" ").map(_.toInt)).map(Order(_)).toArray
	  val producers = orders.filter(_.qty > 0)
	  val consumers = orders.filter(_.qty < 0)
	  
	  val tmin = orders.map(_.start).min
	  val tmax = orders.map(_.end).max
	  
	  // one var for each time slot = the quantity exchanged on that slot
	  val varMapQty = Map[Int,CPVarInt]() 
	  for (t <- tmin to tmax) {
	    val prodUB = producers.map(_.qty.abs).sum
	    varMapQty += (t -> CPVarInt(cp, 0 to prodUB))
	  }
	  
	  // total amount of exchanged quantity
	  val obj: CPVarInt = sum(tmin to tmax)(t => varMapQty(t))	  
	  
	  cp.onSolution {
	    println((tmin to tmax).map(varMapQty(_).getValue).mkString("\t"))
	  }
	  
	  cp.maximize(obj) subjectTo {
	    for (t <- tmin to tmax) {
	        val prodVars = producers.filter(_.overlap(t)).map(_.selected)
	        val prodQty = producers.filter(_.overlap(t)).map(_.qty)
	        val consVars = consumers.filter(_.overlap(t)).map(_.selected)
	        val consQty = consumers.filter(_.overlap(t)).map(_.qty.abs)
	        
	    	cp.add(binaryknapsack(prodVars,prodQty,varMapQty(t)), Strong)
	    	cp.add(binaryknapsack(consVars,consQty,varMapQty(t)), Strong)
	    } 
	  } exploring {
	    val unboundOrders = orders.filter(!_.bound)
	    if (unboundOrders.size == 0) {
	      Branching.noAlternative
	    } else {
	      val order = argMax(unboundOrders)(_.energy).head
	      cp.branchOn(order.selected == 1, order.selected == 0)
	    }
	  }
	  cp.printStats()
	}
}