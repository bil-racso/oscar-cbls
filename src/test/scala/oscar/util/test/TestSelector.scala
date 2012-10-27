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

package oscar.util.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.util._


class TestSelector extends FunSuite with ShouldMatchers {

   
 
    test("test random selector") {
    			
    	val indices: Array[Int] = Array(0,1,2,3,4)
    	
    	val count = Array.fill(indices.size)(0)
 
  
    	for (i <- 0 until 1000) {
    	  val j = select(indices)(_ != 2).get
    	  count(j) += 1
    	}
    	count(0) should be >= (200)
    	count(1) should be >= (200)
    	count(2) should be (0)
    	count(3) should be >= (200)
    	count(4) should be >= (200)
    	count.sum should be(1000)
    	
    }
    
    test("test random min selector") {
    			
    	val indices: Array[Int] = Array(0,1,2,3,4)
    	val f: Array[Int] = Array(1,1,3,2,1)
    	val count = Array.fill(indices.size)(0)
 
    	for (i <- 0 until 1000) {
    	  val j = selectMin(indices)(_ != 2)(f(_)).get
    	  count(j) += 1
    	  
    	}
    	println(count.mkString(","))
    	count(0) should be >= (300)
    	count(0) should be >= (300)
    	count(4) should be >= (300)
    	count(0)+count(1)+count(4) should be(1000)

    }    

}

