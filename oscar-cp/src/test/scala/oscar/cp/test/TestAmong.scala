/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._


class TestAmong extends FunSuite with ShouldMatchers  {
  
  val rand = new scala.util.Random(0)
  
  def amongDecomp(cp: CPSolver, N: CPVarInt, X: Array[CPVarInt], S: Set[Int]) = {
    val counts = Array.fill(S.size)(CPVarInt(0 to X.size)(cp))
    cp.add(gcc(X,S zip counts))
    cp.add(sum(counts) == N)
  }
  
  def nbSol(nmin: Int, nmax: Int, domx: Array[Set[Int]], S: Set[Int], decomp: Boolean = false) = {
	  var nbSol = 0  
	  val cp = CPSolver()
	  
	  val N = CPVarInt(nmin to nmax)(cp)
	  val X = Array.tabulate(domx.size)(i => CPVarInt(domx(i))(cp))
	  cp.solve subjectTo {
	    if (decomp) amongDecomp(cp,N,X,S)
	    else cp.add(new Among(N,X,S))
	  } exploration {
	    cp.binary(X :+ N)
	    nbSol += 1
	  } run()
	  
	  nbSol
  }
  
  def randomDom = Array.fill(10)(rand.nextInt(10)).toSet
  
  
  test("among1") { 
	  val cp = CPSolver()
	  val S = Set(1,2,3)
	  
	  val N = CPVarInt(2)(cp)
	  val X = Array.fill(5)(CPVarInt(0 to 5)(cp))
	  cp.solve subjectTo {
	    cp.add(new Among(N,X,S))
	  } exploration {
	    cp.binary(X)
	    X.map(_.value).count(v => S.contains(v)) should equal(N.value)
	  } run()
	  
  }
  
  
  test("among2") {

    for (i <- 0 to 5) {

      val X = Array.fill(6)(randomDom)
      val S = Set(1, 4, 5)
      nbSol(4, 5, X, S, false) should equal(nbSol(4, 5, X, S, true))
    }

	
  }
  
  // to do, test extreme cases
  
 


}
