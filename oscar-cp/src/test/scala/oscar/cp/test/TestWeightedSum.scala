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


class TestWeightedSum extends FunSuite with ShouldMatchers  {


  test("Weighted Sum 1") {
    val cp = CPSolver()
    var x = Array.tabulate(5)(i => CPVarInt(cp, i))
    val y = weightedSum(0 until 5){i => (i,x(i)) }
    // 0*0 + 1*1 + 2*2 + 3*3 + 4*4
    y.value should be(30)
  }
  
  test("Weighted Sum 2") {
    val cp = CPSolver()
    var x = Array.tabulate(5)(i => CPVarInt(cp, i))
    val y = weightedSum(0 until 5, x)
    // 0*0 + 1*1 + 2*2 + 3*3 + 4*4
    y.value should be(30)
  }
  
  test("Weighted Sum 3") {
    val cp = CPSolver()
    var x = Array.tabulate(2,2)((i,j) => CPVarInt(cp, i*2+j))
    var w = Array.tabulate(2,2)((i,j) => i*2+j)
    // 0*0 + 1*1 + 2*2 + 3*3
    val y = weightedSum(w,x)
    y.value should be(14)
  }
  
  test("Weighted Sum 4") {
    val cp = CPSolver()
    var x = Array.tabulate(2,2)((i,j) => CPVarInt(cp, i*2+j))
    var w = Array.tabulate(2,2)((i,j) => i*2+j)
    // 0*0 + 1*1 + 2*2 + 3*3
    val y = weightedSum(0 until x.size,0 until w.size){case(i,j) => (w(i)(j),x(i)(j))}
    y.value should be(14)
  }
  
  def nbsol(w: Array[Int], domx: Array[Set[Int]], ymin: Int, ymax: Int, decomp: Boolean = false): Int = {
    val cp = CPSolver()
    val x = domx.map(dom => CPVarInt(cp,dom))
    val y = CPVarInt(cp, ymin to ymax)
    var n: Int = 0
    cp.solve subjectTo {
      if (!decomp)
    	  cp.add(weightedSum(w,x,y))
      else 
        cp.add(sum(w.zip(x).map{case(wi,xi) => xi*wi}) == y)
    } exploration {
      cp.binaryFirstFail(x)
      n +=  1
    } run()
    n
  }
  
  
  
  
  test("Weighted Sum 5") {
    val x = Array(Set(1,2),Set(4,6,8),Set(-6,-4,0,6))
    val w = Array(4,-3,2)
    println("=>"+nbsol(w,x,-100,100,true))
    println("=>"+nbsol(w,x,-100,100,false))
  }    
  
 
}
  
  
