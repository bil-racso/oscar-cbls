/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._


class TestGCC extends FunSuite with ShouldMatchers  {
	val rand = new scala.util.Random()
  
    def randomDom(n: Int): Array[Array[Int]] = {
	  val low = Array.tabulate(n)(i => rand.nextInt(2))
	  val up = Array.tabulate(n)(i => low(i)+rand.nextInt(2))
	  Array(low,up)
	}
	
	def randomOcc(n: Int): Array[Array[Int]] = {
	  val low = Array.tabulate(n)(i => rand.nextInt(1))
	  val up = Array.tabulate(n)(i => low(i)+rand.nextInt(3))
	  Array(low,up)
	}
	
	
	/**
     * return the number of sol of the constraints
     */
   def nbSolution(randomDom: Array[Array[Int]], randomOcc: Array[Array[Int]], gccvar: Boolean): Int = {
    	val cp = CPSolver()
    	val x: Array[CPVarInt] = Array.tabulate(randomDom(0).size) (i => CPVarInt(cp,randomDom(0)(i) to randomDom(1)(i)))
    	val o: Array[CPVarInt] = Array.tabulate(randomOcc(0).size) (i => CPVarInt(cp,randomOcc(0)(i) to randomOcc(1)(i)))
    	 
    	
    	var nb = 0
    	
    	if (gccvar) {
    		  cp.post(new oscar.cp.constraints.GCCVar(x,-1,o));
    	} else {
    		  cp.post(new oscar.cp.constraints.SoftGCC(x,-1,randomOcc(0),randomOcc(1),CPVarInt(cp,0)));
    	}
        if (cp.isFailed()) {
    		  return -1;
    	}

    	cp.solve exploration {
    	    cp.binary(x)
    	    if (gccvar) o.forall(_.isBound) should be(true)
    	    nb += 1
    	} run()
    	nb
  }
	


  test("GCC1") {
    for (i <- 0 until 150) {
      val randDom = randomDom(3)
      val randOcc = randomOcc(4)
      nbSolution(randDom,randOcc,true) should be(nbSolution(randDom,randOcc,false))
    }
  }  
  
  test("GCC2") {  	
    	
    	val cp = CPSolver()
    	
    	val x = Array.fill(10)(CPVarInt(cp,0 to 10))
    	
    	for (i <- 0 until 2; v <- 0 until 5) {
    	  cp.post(x(i*5+v) == v)
    	}
       	
    	val o = Array.fill(10)(CPVarInt(cp,0 to 10))
    	
    	cp.post(new oscar.cp.constraints.GCCVar(x, 0, o));
    
    	cp.isFailed() should be(false)
    	
    	for (i <- 0 until o.size) {
    	  o(i).isBound should be(true)
    	  if (i < 5) o(i).value should be(2)
    	  else o(i).value should be(0)
    	}
  }
  
 

}
