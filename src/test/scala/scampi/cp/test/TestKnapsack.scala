/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/

package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

/**
 * author Pierre Schaus pschaus@gmail.com
 */
class TestKnapsack extends FunSuite with ShouldMatchers with CPModel {
  
  def solve(n: Int, cons: Boolean, seed: Int = 0) : Int = { 
	  val rand = new scala.util.Random(seed)
	  val u = 40
	  val profit = Array.fill(n)(1+rand.nextInt(100))
	  val weight = Array.fill(n)(1+rand.nextInt(u))
	  val cp = CPSolver()
	  val P = new CPVarInt(cp,0 to 1000000)
	  val W = new CPVarInt(cp,0 to (n/2 * u/2))
	  val X = Array.fill(profit.size)(new CPVarBool(cp))
	  var obj = 0
	  cp.maximize(P) subjectTo {
	    cp.add(binaryknapsack(X,profit,P))
	    cp.add(binaryknapsack(X,weight,W))
	    if (cons) cp.add(new Knapsack(X,profit,weight,P,W,true))
	  } exploration {
	    while(!cp.allBounds(X)) {
	      val (x,i) = X.zipWithIndex.filter{case (x,i) => !x.isBound}.maxBy{case (x,i) => weight(i)}
	      cp.branch(cp.post(x == 1))(cp.post(x == 0))
	    }
	    obj = P.getValue()
	  }
	  obj
  }


  test("Knapsack 1") {
    for (i <- 0 to 20) {
	 val res1 = solve(20,false,i)
	 val res2 = solve(20,true,i)
	 res1 should be (res2)
	}
  }
  
  test("Knapsack 2") {
    val w = Array(25,2,32,36,36)
    val p = Array(76,62,4,91,94)
    val cp = CPSolver()
    val X = Array.fill(w.size)(CPVarBool(cp))
    val P = new CPVarInt(cp,155 to 170)
    val W = new CPVarInt(cp,0 to 40)
    cp.add(new Knapsack(X,p,w,P,W))
    cp.add(X(3) == 0)
    X(0).getValue should be (0)
    X(1).getValue should be (1)
    X(2).getValue should be (0)
    X(3).getValue should be (0)
    X(4).getValue should be (1)
    P.getValue should be (156)
    W.getValue should be (38)
    cp.getStatus() should be (CPOutcome.Suspend)

  }
  
  test("Knapsack 3") {
  
    val p = Array(57,85,71,24,33)
    val w = Array(10,40,37,21,30)
    val cp = CPSolver()
    val X = Array.fill(w.size)(CPVarBool(cp))
    val P = new CPVarInt(cp,85 to 170)
    val W = new CPVarInt(cp,0 to 40)
    cp.add(X(1) == 0)
    cp.add(new Knapsack(X,p,w,P,W))
    X(0).getValue should be (1)
    X(1).getValue should be (0)
    X(2).getValue should be (0)
    X(3).getValue should be (0)
    X(4).getValue should be (1)
    P.getValue should be (90)
    W.getValue should be (40)
    cp.getStatus() should be (CPOutcome.Suspend)

  }  
  

  
  

  
  
  

}