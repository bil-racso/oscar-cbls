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
import oscar.visual._
import oscar.cp.core._
import scala.io.Source
import scala.collection.mutable.Map
import oscar.search.Branching

/** 
 * Game invented by Bertrand Cornelusse and Gilles Scouvart for the 10 years of n-Side:
 * Maximize the total market exchange such that demand and supply match at any time
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElectricityMarket extends CPModel {
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

	  }
	  
	  val firstLine::restLines = Source.fromFile("data/electricityMarket.txt").getLines.toList
	  val n = firstLine.toInt
	  
	  val orders = restLines.map(_.split(" ").map(_.toInt)).map(Order(_)).toArray
	  val producers = orders.filter(_.qty > 0)
	  val consumers = orders.filter(_.qty < 0)
	  
	  val tmin = orders.map(_.start).min
	  val tmax = orders.map(_.end).max
	  
	  
	  // -------------visual components ------------
	  val f = new VisualFrame("Electricity Market")
		// creates the plot and place it into the frame
	  val plot = new Plot2D("","Solution number","Qty")
	  f.createFrame("Objective").add(plot)
	  val barPlot = new BarChart("","Time","Qty",tmax-tmin+1)	
	  f.createFrame("Qty Exchange").add(barPlot)
	  f.pack()
	  // ------------------------------------------
	  
	  
	  // one var for each time slot = the quantity exchanged on that slot
	  val varMapQty = Map[Int,CPVarInt]() 
	  for (t <- tmin to tmax) {
	    val prodUB = producers.map(_.qty.abs).sum
	    varMapQty += (t -> CPVarInt(cp, 0 to prodUB))
	  }
	  var nbSol = 0
	  // total amount of exchanged quantity
	  val obj: CPVarInt = sum(tmin to tmax)(t => varMapQty(t))
	  
	  cp.maximize(obj) subjectTo {
	    for (t <- tmin to tmax) {
	        val prodVars = producers.filter(_.overlap(t)).map(_.selected)
	        val prodQty = producers.filter(_.overlap(t)).map(_.qty)
	        val consVars = consumers.filter(_.overlap(t)).map(_.selected)
	        val consQty = consumers.filter(_.overlap(t)).map(_.qty.abs)
	        
	    	cp.add(binaryknapsack(prodVars,prodQty,varMapQty(t)), Strong)
	    	cp.add(binaryknapsack(consVars,consQty,varMapQty(t)), Strong)
	    } 
	  } exploration {
	    cp.binary(orders.map(_.selected))
	    /*
	    // efficient heuristic
	    def allBounds = orders.filter(!_.bound).isEmpty
	    while (!allBounds) {
	      val unboundOrders = orders.filter(!_.bound)
	      val order = argMax(unboundOrders)(_.energy).head
	      cp.branch {cp.post(order.selected == 1)} {cp.post(order.selected == 0)}
	    }
	    */
	    // update visualization
	    for (t <- tmin to tmax) {
	      barPlot.setValue(t-tmin,varMapQty(t).getValue())
	    }
	    plot.addPoint(nbSol,obj.getValue())
	    nbSol += 1
	    
	  }
	  cp.printStats()
	  
	}
}
