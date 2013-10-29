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
class TestSetDiff extends FunSuite with ShouldMatchers {
  
  val rand = new scala.util.Random(0)

  class SetDiffDecomp(val a: CPVarSet, val b: CPVarSet, val c: CPVarSet) extends Constraint(a.s, "SetDiffDecomp") {

    override def setup(l: CPPropagStrength): CPOutcome = {

      if (!a.isBound) a.callPropagateWhenDomainChanges(this)

      if (!b.isBound) b.callPropagateWhenDomainChanges(this)

      if (!c.isBound) c.callPropagateWhenDomainChanges(this)

      return propagate()
    }

    override def propagate(): CPOutcome = {
      // --------------------------------------------

      // initial filtering   a - b = c

      // for every value in possible a, if not in c and not required in b, remove it from a
      // for every value in required a, if not in b, required in c
      for (v <- a.possibleNotRequiredValues.toSet; if !c.isPossible(v) && !b.isRequired(v)) {
        if (a.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
      for (v <- a.requiredValues; if !b.isPossible(v)) {
        if (c.requires(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }

      // for every value v in required b => remove it from c

      for (v <- b.requiredValues) {
        if (c.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }

      // for every value in possible c if not in a possible, remove it from c
      // for every value v in required c => must be required in a and excluded from b   
      for (v <- c.possibleNotRequiredValues.toSet; if !a.isPossible(v)) {
        if (c.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
      for (v <- c.requiredValues) {
        if (a.requires(v) == CPOutcome.Failure || b.excludes(v) == CPOutcome.Failure) {
          return CPOutcome.Failure
        }
      }
      CPOutcome.Suspend
    }
  }
  
  def solCount(aReq: Set[Int], aPoss: Set[Int], bReq: Set[Int], bPoss: Set[Int], cReq: Set[Int], cPoss: Set[Int], decomp: Boolean = false): Int = {
    val cp = CPSolver()
    var a = CPVarSet(cp, aReq, aPoss)
    var b = CPVarSet(cp, bReq, bPoss)
    var c = CPVarSet(cp, cReq, cPoss)
    var nbSol = 0
    val oc = 
      if (decomp) cp.post(new SetDiffDecomp(a, b, c))
      else cp.post(new SetDiff(a, b, c))
    //println("fix point a:" + a + " b:" + b + " c:" + c)
    if (oc == CPOutcome.Failure) return 0
    cp exploration {
      cp.binary(a)
      cp.binary(b)
      cp.binary(c)
      nbSol += 1
    } run ()
    nbSol
  }

  // a - b = c

  test("Test SetDiff 1") {
    var nbSol = 0
    val cp = CPSolver()
    var a = CPVarSet(cp, Set(1, 4), Set(5, 7, 8))
    var b = CPVarSet(cp, Set(4), Set(5))
    var c = CPVarSet(cp, Set(), Set(1, 2, 3, 4, 5))

    cp.add(new SetDiff(a, b, c))

    a.isPossible(7) should be(false)
    a.isPossible(8) should be(false)
    b.possibleSize should be(2)
    c.isRequired(1) should be(true)
    c.isPossible(5) should be(true)
    c.possibleSize should be(2)
    
    cp.exploration {
      cp.binary(a)
      cp.binary(b)
      cp.binary(c)
      nbSol += 1
    } run ()

    nbSol should be(4)
  }

  test("Test SetDiff 2") {
    val aReq = Set(1,4,8)
    val aPoss = Set(9,100,888)
    val bReq = Set(9,100,888)
    val bPoss = Set(1,4,8)
    val cReq = Set(1)
    val cPoss = Set(4,8,9,100,888)
    solCount(aReq,aPoss,bReq,bPoss,cReq,cPoss,true) should equal(solCount(aReq,aPoss,bReq,bPoss,cReq,cPoss,false))
  }
  
  def randSet(): (Set[Int],Set[Int]) = {
    val aReq = (0 to 3).map(i =>rand.nextInt(20)).toSet
    val aPoss = (0 to 20).map(i => rand.nextInt(20)).toSet.filterNot(aReq.contains(_))
    (aReq,aPoss)
    
  }
  
  test("Test SetDiff 3") {
    val aReq = Set(1,4,8)
    val aPoss = Set(9,100,888)
    val bReq = Set(9,100,888)
    val bPoss = Set(1,4,8)
    val cReq = Set(1)
    val cPoss = Set(4,8,9,100,888)
    solCount(aReq,aPoss,bReq,bPoss,cReq,cPoss,true) should equal(solCount(aReq,aPoss,bReq,bPoss,cReq,cPoss,false))
  } 
  
  test("Test SetDiff 4") {

    for (i <- 0 until 40) {
      
      val (aReq, aPoss) = randSet()
      val (bReq, bPoss) = randSet()
      val (cReq, cPoss) = randSet()
      
      val n1 = solCount(aReq, aPoss, bReq, bPoss, cReq, cPoss, true)
      val n2 = solCount(aReq, aPoss, bReq, bPoss, cReq, cPoss, false)
      //println(n1 + "?=?" + n2)
      n1 should equal(n2)
    }

  }

}
  
