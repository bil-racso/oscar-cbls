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



package oscar.cp.constraints

import oscar.cp.core.CPSetVar
import oscar.algo.reversible.ReversibleDouble
import oscar.cp.modeling._
import oscar.cp.core._
import scala.io.Source
import oscar.algo.reversible.ReversibleInt
import oscar.algo.DisjointSets
import CPOutcome._
import scala.collection.mutable.ArrayBuffer

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class HeldKarp(val edges: CPSetVar,val edgeData: Array[(Int,Int,Int)], val cost: CPIntVar) extends Constraint(edges.store) {

  val epsilon = 10e-6
  val n = (edgeData.map(_._1).max max (edgeData.map(_._2).max)) +1
  val component = new DisjointSets(0,n-1)
  val distMatrix = Array.fill(n,n)(new ReversibleInt(s,Int.MaxValue)) 
  

  
  val edgeIndex = Array.fill(n,n)(-1)
  val y = Array.fill(n)(0.0)//(new ReversibleDouble(s,0.0))
  
  for (((i,j,w),idx) <- edgeData.zipWithIndex) {
    edgeIndex(i)(j) = idx
    edgeIndex(j)(i) = idx
    distMatrix(i min j)(i max j) := w
  }
  
  def edgeWeight(i: Int, j: Int): Int = {
    distMatrix(i min j)(i max j).value
  }
  

  def removeEdge(i: Int,j: Int): CPOutcome = {
    distMatrix(i min j)(i max j) := Int.MaxValue
    edges.excludes(edgeIndex(i)(j))
  }
  
  def forceEdge(i: Int,j: Int): CPOutcome = {
    edges.requires(edgeIndex(i)(j))
  }  
  
  def isEdgePossible(i: Int,j: Int): Boolean = {
    edges.isPossible(edgeIndex(i)(j))
  }
  
  def isEdgeRequired(i: Int,j: Int): Boolean = {
    edges.isRequired(edgeIndex(i)(j))
  }  

  override def setup(l: CPPropagStrength): CPOutcome = {
    println("setup")
    for (((i,j,w),idx) <- edgeData.zipWithIndex) {
      if (!edges.isPossible(idx)) removeEdge(i,j)
      if (edges.isRequired(idx)) forceEdge(i,j)
    }
    edges.callPropagateWhenDomainChanges(this)
    val oc = propagateNumSteps(1000)
    //println("lb init:"+cost)
    return oc
  }
  
  override def propagate(): CPOutcome = {
    propagateNumSteps(15)
  }
  
  def propagateNumSteps(nSteps: Int): CPOutcome = {
    var iter = 0
    var improvement = true
    var lb = 0
    var stepSize = 0.1
    var alpha = 2.0
    var beta = 0.5
    
    for (metaiter <- 0 until 5) {
      iter = 0
      while (iter < nSteps) {
        
        iter += 1
        improvement = false
        val excluded = 0
        component.reset()
        val incident = Array.fill(n)(0)

        def edgeWeight(idx: Int): Double = {
          val (i, j, w) = edgeData(idx)
          (w - y(i) - y(j))
        }

        val required = edges.requiredSet()
        var weight = 0.0
        var nAdjacentToExcluded = 0
        for (idx <- required) {
          val (i, j, w) = edgeData(idx)
          if (i != excluded && j != excluded) {
            component.union(component.find(i), component.find(j))
            incident(i) += 1
            incident(j) += 1
          } else {
            nAdjacentToExcluded += 1
          }
          weight += edgeWeight(idx)
        }

        if (nAdjacentToExcluded > 2) {
          return Failure
        }

        val possible = edges.possibleNotRequiredValues.toArray.sortBy(i => edgeWeight(i))

        for (idx <- possible) {
          val (i, j, w) = edgeData(idx)
          if (i != excluded && j != excluded) {
            if (component.find(i) != component.find(j)) {
              incident(i) += 1
              incident(j) += 1
              component.union(component.find(i), component.find(j))
              weight += edgeWeight(idx)
            }
          } else {
            if (nAdjacentToExcluded < 2) {
              incident(i) += 1
              incident(j) += 1
              weight += edgeWeight(idx)
              nAdjacentToExcluded += 1
            }
          }
        }
        //println((2 * y.map(_ + 0.0).sum + weight))
        val oneTreeLB = ((2 * y.map(_ + 0.0).sum + weight)-epsilon).ceil.toInt
        if (lb < oneTreeLB) {
          improvement = true
          lb = oneTreeLB
          
          if (cost.updateMin(lb) == Failure) {
            return Failure
          }

          //println(incident.mkString(","))
        }
        val denom: Double = (for (i <- 0 until n) yield ((2 - incident(i)) * (2 - incident(i)))).sum
         
        
        var target = if (cost.max - oneTreeLB < 0) oneTreeLB+0.1 else cost.max
        if (denom == 0) stepSize = 0
        else stepSize = alpha * (target - oneTreeLB) / denom
        //stepSize = alpha * (cost.max - oneTreeLB) / denom
        //println("stepSize:"+stepSize+ "denom:"+denom)
        
        //stepSize = 20 * Math.pow(rho, iter)
        //println("stepSize:"+stepSize)

        for (i <- 0 until n) {
          y(i) += (stepSize * (2 - incident(i)))
        }
      }
      // fini les iters
      alpha *= beta;
      beta /= 2;
      

    }
    /*
    if (lb > cost.min) {
      
      println("iter:"+iter+" lb:"+lb+" cost:"+cost)
    }*/
    if (cost.updateMin(lb) == Failure) {
      return Failure
    } 
    return Suspend
    
    
  }
 
}

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class ChannelTSP(val succ: Array[CPIntVar],val distMatrix: Array[Array[Int]]) extends Constraint(succ(0).store) {
  
  val n = succ.size
  
  protected val edges = ((for (i <- 0 until n; j <- succ(i); if (i != j)) yield (n+i,j,distMatrix(i)(j))) ++ 
              (for (i <- 0 until n) yield (i+n,i,0))).toArray
  
  val edgeIndex = Array.fill(n,n)(0)
  for (((i,j,w),idx) <- edges.zipWithIndex) {
    edgeIndex(i-n)(j) = idx
  }
  
  protected val edgeVar = CPSetVar((0 until edges.size).toSet)(s)
  // todo: fix the cardinality of the set            
  
  
  override def setup(l: CPPropagStrength): CPOutcome = {
	s.post(edgeVar.card == 2*n)
    
    for (i <- 0 until n) {
      succ(i).callValRemoveIdxWhenValueIsRemoved(this,i)
      succ(i).callValBindIdxWhenBind(this,i)
    }
    edgeVar.callValExcludedWhenExcludedValue(this)
    edgeVar.callValRequiredWhenRequiredValue(this)
    
    for ((i,j,w) <- edges) {
      if (((i-n) != j) && !succ(i-n).hasValue(j)) {
        edgeVar.excludes(edgeIndex(i-n)(j))
      }
    }

    CPOutcome.Suspend
  }
  
  override def valRemoveIdx(x: CPIntVar, idx: Int, v: Int): CPOutcome = {
    //println("valRemoveIdx")
    
    if (v != idx) {
      edgeVar.excludes(edgeIndex(idx)(v))
    }
    else CPOutcome.Suspend
  }
  
  override def valBindIdx(x: CPIntVar, idx: Int): CPOutcome = {
    //println("valBindIdx")
    edgeVar.requires(edgeIndex(idx)(x.value))
  } 
  
  override def valExcluded(x: CPSetVar, v: Int): CPOutcome = {
    //println("valExcluded")
    val (i,j,w) = edges(v)
    succ(i-n).removeValue(j)
  }  

  override def valRequired(x: CPSetVar, v: Int): CPOutcome = {
    val (i,j,w) = edges(v)
    //println("valRequired "+v+" =edge:"+((i-n),j))
    if ((i-n) != j) {
      succ(i-n).assign(j)
    } else CPOutcome.Suspend
  }   

}


object HeldKarp extends App {
  
  implicit val cp = new CPSolver()
  val edgeData = Array((0,1,5),(1,2,5),(2,3,5),(3,0,3))
  val cost = CPIntVar(0 to 1000)
  val edges = CPSetVar(Set(0,1,2,3),Set())
  cp.add(new HeldKarp(edges,edgeData,cost))
  println(cost)
  
  
}
