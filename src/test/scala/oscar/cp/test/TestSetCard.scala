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

/**
 * @author: Pierre Schaus pschaus@gmail.com
 */
class TestSetCard extends FunSuite with ShouldMatchers  {


  test("Test SetCard 1") {
    var nbSol = 0
    val cp = CPSolver()
    var x = new CPVarSet(cp, 1 , 5)
    val card = CPVarInt(cp,1 to 2)
    cp.post(new SetCard(x,card))
    cp.exploration {
      println("exploration")
      cp.binary(x)
      println(x.value)
      nbSol +=1
    } run()
    nbSol should be(15)
  }
  
  test("Test SetCard 2") {
    val cp = CPSolver()
    val x = CPVarSet(cp,Set(1,3),Set(4))
    cp.post(new SetCard(x,CPVarInt(cp,2)))
    x.isBound should be(true)
  }
  
  test("Test SetCard 3") {
    val cp = CPSolver()
    val x = CPVarSet(cp,Set(1,3),Set(4))
    cp.post(new SetCard(x,CPVarInt(cp,3)))
    x.isBound should be(true)
    x.value.size should be(3)
  } 
  
  
  test("Test SetCard 4") {
    val cp = CPSolver()
    val x = CPVarSet(cp,Set(1,3),Set(4))
    val c = CPVarInt(cp,-100,100)
    cp.post(new SetCard(x,c))
    c.min should be(2)
    c.max should be(3)
  }  
  
  
}
  
