/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/

package scampi.cp.constraints


import scampi.cp.core._
import scampi.reversible._
import scampi.cp.core.CPOutcome
import scampi.cp.modeling._
import scala.collection.JavaConversions._


/**
 * Implementation of Knapsack Constraint
 */
class Knapsack(val X: Array[CPVarBool], val profit: Array[Int], val weight: Array[Int], val P: CPVarInt, val W: CPVarInt, val filter: Boolean = true ) extends Constraint(X(0).getStore(), "Table2") with Constraints {

  // sort items by efficiency, tie break on the weights
  
  val efficiencyPerm = (0 until X.size).sortBy(i => (-profit(i).toDouble/weight(i), -weight(i)))
  val x = efficiencyPerm.map(X(_))
  val p = efficiencyPerm.map(profit(_))
  val w = efficiencyPerm.map(weight(_))
  val unbound = new ReversibleOrderedSet(s,0,x.size-1)
  val packedWeight = new ReversibleInt(s,0)
  val packedProfit = new ReversibleInt(s,0)
  
  val weightPerm = (0 until x.size).sortBy(i => w(i))
  
  
  override def setup(l: CPPropagStrength): CPOutcome = {    
    setIdempotent() 
    if (s.post(binaryknapsack(X,profit,P)) == CPOutcome.Failure) {
      return CPOutcome.Failure;
    }
    if (s.post(binaryknapsack(X,weight,W)) == CPOutcome.Failure) {
    		return CPOutcome.Failure;
    }
    if (filter) {
    	x.filter(!_.isBound()).foreach(_.callPropagateWhenDomainChanges(this))
    	for ((y,i) <- x.zipWithIndex) {
    		val ok = if (y.isBound()) valBindIdx(y,i) else y.callValBindIdxWhenBind(this,i)
    		if (ok == CPOutcome.Failure) return CPOutcome.Failure
    	}
    	if (propagate() == CPOutcome.Failure) return CPOutcome.Failure
    	P.callPropagateWhenMinChanges(this)
    	W.callPropagateWhenMaxChanges(this)
    }
    CPOutcome.Suspend
  }

  
  override def valBindIdx(y: CPVarInt, i: Int) : CPOutcome = {
    unbound.removeValue(i);
    if (y.getValue() == 1) {
    	// add this to the capacity and to the reward
        packedProfit.value = packedProfit.value + p(i)  
        packedWeight.value = packedWeight.value + w(i)  
    }
    return CPOutcome.Suspend
  }
  
  /**
   * return (profit,weight,s) of already packed items and unbound items up to s-1
   */
  private def getCriticalItem() : (Int,Int,Int) = {
    var profit = packedProfit.value
    var weight = packedWeight.value
        
    val ite = unbound.iterator()
    var s = -1 // critical item index
    while (ite.hasNext() && s < 0) {
      val i = ite.next()
      if (weight + w(i) <= W.getMax()) {
        weight += w(i)
        profit += p(i)
      } else {
        // reached the critical item, take a fraction of it to reach max capa
        s = i
      }
    }
    return (profit,weight,s)
  }
  
 
  override def propagate(): CPOutcome = {
    //println("progagate")
    // try to find the maximum profit under the weight/capa constraint using the linear relaxation
    val (profit,weight,s) = getCriticalItem()
    
    if (s == -1) { // enough capa to take all items
      if (P.updateMax(profit) == CPOutcome.Failure) return CPOutcome.Failure
      else return CPOutcome.Suspend
    } else {
      val weightSlack = W.getMax() - weight
      val maxProfit = profit + Math.floor(weightSlack.toDouble / w(s) * p(s)).toInt
      if (P.updateMax(maxProfit) == CPOutcome.Failure) return CPOutcome.Failure
      
      //for (i <- packedWeight)
      
      
      
      return CPOutcome.Suspend
    }

    
    return CPOutcome.Suspend
  }

}

object Knapsack {
  def main(args: Array[String]) {
	  val rand = new scala.util.Random(0)
	  val n = 30
	  val u = 40
	  val profit = Array.fill(n)(rand.nextInt(100))
	  val weight = Array.fill(n)(rand.nextInt(u))
	  val cp = CPSolver()
	  val P = new CPVarInt(cp,0 to Int.MaxValue)
	  val W = new CPVarInt(cp,0 to (n/2 * u/2))
	  val X = Array.fill(profit.size)(new CPVarBool(cp))
	  
	  cp.maximize(P) subjectTo {
	    cp.add(new Knapsack(X,profit,weight,P,W,true))
	  } exploration {
	    while(!cp.allBounds(X)) {
	      val (x,i) = X.zipWithIndex.filter{case (x,i) => !x.isBound}.maxBy{case (x,i) => weight(i)}
	      cp.branch(cp.post(x == 1))(cp.post(x == 0))
	    }
	  }
	  
	  cp.printStats()

      

  }
}


