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
import scala.io.Source
import scala.collection.mutable.Map
/** 
 * Game invented by Bertrand Cornelusse and Gilles Scouvart for the 10 years of n-Side:
 * Maximize the total market exchange such that demand and supply match at any time
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElectricityMarket extends MIPModel {
	def main(args: Array[String]) {
	  

	  // format is : qty ( > 0 if producer < 0 if consumer) start end
	  val firstLine::restLines = Source.fromFile("data/electricityMarketEasy.txt").getLines.toList
	  val n = firstLine.toInt
	  
	  val orders = restLines.map(_.split(" ").map(_.toInt))
	  val producers = orders.filter(_(0) > 0)
	  val consumers = orders.filter(_(0) < 0)
	  
	  val tmin = orders.map(_(1)).min
	  val tmax = orders.map(_(2)).max
	  
	  // one variable for each order if we take it or not
	  val mip = MIPSolver(LPSolverLib.glpk)
	  val varMap = Map[Array[Int],MIPVar]() 
	  orders.foreach(o => varMap += (o -> MIPVar(mip,"order",0 to 1)))
	  
	  // helper functions
	  def overlap(order: Array[Int], t: Int) = t <= order(2) && t >= order(1) 
	  def vars(l: List[Array[Int]], t: Int) = l.filter(overlap(_,t)).map(varMap(_)).toArray
	  def qty(l: List[Array[Int]], t: Int) = l.filter(overlap(_,t)).map(_(0).abs).toArray


	  val t0 = System.currentTimeMillis
	  mip.maximize(sum(producers)(order => varMap(order)*(order(0) * (order(2)-order(1)+1)))) subjectTo {
	    for (t <- tmin to tmax) {
	    	val prod_t = producers.filter(overlap(_,t))
	    	val cons_t = consumers.filter(overlap(_,t))  
	    	if (!prod_t.isEmpty && !cons_t.isEmpty) {
	    	    // production and consumption must be the same at time t
	    		mip.add(sum(prod_t)(p => varMap(p)*(p(0).abs)) == sum(cons_t)(c => varMap(c)*(c(0).abs)))
	    	}
	    } 
	  }
	  println("time:"+(System.currentTimeMillis-t0))
	  println("objective:"+mip.getObjectiveValue)
	  val check = Array.tabulate(tmax-tmin+1)(_=> 0)
	  for (o <- orders; if (varMap(o).getValue > 0.5)) {
	      for (t <- o(1) to o(2)) {
	        check(t-1) += o(0)
	      }
	  }
	  
	  println("checker:"+check.mkString(","))
	  
	}
}
