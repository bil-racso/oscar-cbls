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
import oscar.algo.search.SearchStatistics


class TestHeldKarp extends FunSuite with ShouldMatchers  {

  class MinCircuit(succ: Array[CPIntVar], distMatrix: Array[Array[Int]], cost: CPIntVar) extends ChannelTSP(succ, distMatrix) {

    override def setup(l: CPPropagStrength): CPOutcome = {
      //if (s.post(circuit(succ),l) == Failure) return Failure
      if (s.post(new HeldKarp(edgeVar, edges, cost)) == CPOutcome.Failure) return CPOutcome.Failure
      super.setup(l)
    }

  }  


  def bestTourSize(distMatrix: Array[Array[Int]]): Int = {
    val cp = CPSolver()
    cp.silent = true
    val n = distMatrix.size
    
    val succ = Array.fill(n)(CPIntVar(0 until n)(cp))
    cp.add(circuit(succ),Strong)
    val obj = sum(0 until n)(i => distMatrix(i)(succ(i)))
    cp.minimize(obj)
    cp.search(binaryFirstFail(succ))
    var best = Int.MaxValue
    cp.onSolution {best = obj.value}
    cp.start()
    best
  }
  
  def stat(distMatrixSucc: Array[Array[Int]], ub: Int,minAss: Boolean,heldKarp: Boolean): SearchStatistics = {
    val n = distMatrixSucc.size    
    val distMatrixPred = Array.tabulate(n,n)((i,j) => distMatrixSucc(j)(i))    
    
    implicit val cp = CPSolver()
    

    
    val succ = Array.fill(n)(CPIntVar(0 until n))
    val pred = Array.fill(n)(CPIntVar(0 until n))

    add(circuit(pred),Strong)
    add(circuit(succ),Strong)   
    
    add(new Inverse(pred,succ))

    val obj = sum(0 until n)(i => distMatrixSucc(i)(succ(i)))
    
    add(sum(0 until n)(i => distMatrixPred(i)(pred(i))) == obj)
    add(obj <= ub)

    if (minAss) {
      add(minAssignment(succ, distMatrixSucc, obj))
      add(minAssignment(pred, distMatrixPred, obj))
    }   
    
    if (heldKarp) {
      add(new MinCircuit(succ,distMatrixSucc,obj),Strong)
      //add(new MinCircuit(pred,distMatrixPred,obj),Strong)     
    }    
    
    cp.search(binaryStatic(succ,_.min))
    val stat = cp.start()
    stat
  } 
  /*
  test("HK1") {
    println("here")

      val distMatrix = Array(Array(0,3,8),
                             Array(5,0,5),
                             Array(5,0,0))
      val b = bestTourSize(distMatrix)
      println(b)
      val s1 = stat(distMatrix, b + 1, true, false)
      val s2 = stat(distMatrix, b + 1, true, true)
      if (s1.nSols != s2.nSols) {
        distMatrix.foreach(a => println(a.mkString("\t")))
      }
      println(s1.nSols + " " + s2.nSols)
      s1.nSols should be(s2.nSols)
    
  } */  
  

  
  test("HK") {
    println("here")
    val rand = new scala.util.Random(0)

    for (i <- 0 until 100) {
      val distMatrix = Array.fill(6, 6)(rand.nextInt(10))
      val b = bestTourSize(distMatrix)
      val s1 = stat(distMatrix, b + 1, true, false)
      val s2 = stat(distMatrix, b + 1, true, true)
      if (s1.nSols != s2.nSols) {
        distMatrix.foreach(a => println(a.mkString("\t")))
      }
      //println(s1.nSols + " " + s2.nSols)
      s1.nSols should be(s2.nSols)
    }
  } 

}
