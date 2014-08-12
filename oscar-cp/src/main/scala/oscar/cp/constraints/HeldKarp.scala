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
import oscar.algo.CCTree
import oscar.algo.CCTreeNode
import oscar.algo.RangeMinQuery

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class HeldKarp(val edges: CPSetVar,val edgeData: Array[(Int,Int,Int)], val cost: CPIntVar) extends Constraint(edges.store) {

  val epsilon = 10e-6
  val n = (edgeData.map(_._1).max max (edgeData.map(_._2).max)) +1
  val component = new DisjointSets[CCTreeNode](0,n-1)
  val cctree = new CCTree(n-1)
  val distMatrix = Array.fill(n,n)(new ReversibleInt(s,Int.MaxValue)) 
  

  
  val edgeIndex = Array.fill(n,n)(-1)
  val y = Array.fill(n)(0.0)
  
  
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
    for (((i,j,w),idx) <- edgeData.zipWithIndex) {
      if (!edges.isPossible(idx)) removeEdge(i,j)
      if (edges.isRequired(idx)) forceEdge(i,j)
    }
    edges.callPropagateWhenDomainChanges(this)
    val oc = propagateNumSteps(100)
    return oc
  }
  
  override def propagate(): CPOutcome = {
    propagateNumSteps(5)
  }
  
  def propagateNumSteps(nSteps: Int): CPOutcome = {
    var iter = 0
    var improvement = true
    var lb = 0
    var stepSize = 0.1
    var alpha = 2.0
    var beta = 0.5
    val excluded = n-1
    
    for (metaiter <- 0 until 3) {
      iter = 0
      while (iter < nSteps) {
        
        iter += 1
        improvement = false
        
        component.reset()
        cctree.reset()
        component.resetAndSetData(i => cctree.nodes(i))
        val edgeUsed = Array.fill(edgeData.length)(false)
          
        val incident = Array.fill(n)(0)

        def edgeWeight(idx: Int): Double = {
          val (i, j, w) = edgeData(idx)
          (w - y(i) - y(j))
        }
        // first add the required edges to the tree
        val required = edges.requiredSet()
        var weight = 0.0
        var nAdjacentToExcluded = 0
        for (idx <- required) {
          val (i, j, w) = edgeData(idx)
          incident(i) += 1
          incident(j) += 1
          if (i != excluded && j != excluded) {
            val t1 = component.find(i).data.get
            val t2 = component.find(j).data.get
            val t = cctree.merge(t1,t2,idx)
            component.union(i,j,t)
            edgeUsed(idx) = true
          } else {
            nAdjacentToExcluded += 1
          }
          if (incident(i) > 2 || incident(j) > 2) return Failure
          weight += edgeWeight(idx)
        }
        // check if out degree is not more than 2
        if (nAdjacentToExcluded > 2) {
          return Failure
        }

        // then complete the minimum spanning tree with Kruskal
        val possible = edges.possibleNotRequiredValues.toArray.sortBy(i => edgeWeight(i))
        for (idx <- possible) {
          val (i, j, w) = edgeData(idx)
          if (i != excluded && j != excluded) {
            if (component.find(i) != component.find(j)) {
              incident(i) += 1
              incident(j) += 1
              val t1 = component.find(i).data.get
              val t2 = component.find(j).data.get
              val t = cctree.merge(t1, t2, idx)
              component.union(i, j, t)   
              edgeUsed(idx) = true
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
        val oneTreeLBf = ((2 * y.map(_ + 0.0).sum + weight)-epsilon)
        val oneTreeLB = oneTreeLBf.ceil.toInt
        if (lb < oneTreeLB) {
          improvement = true
          lb = oneTreeLB
          
          if (cost.updateMin(lb) == Failure) {
            return Failure
          }
        }
        if (!cctree.singleRoot) {
          //println("=> problem, not single root")
          // the graph without "excluded" is not connected
          return Failure
        }

        // filtering of the edges
        if (iter == nSteps && false) {
          val inorder = cctree.inorderCollect()
          val pos = Array.fill(inorder.length)(0)
          for (i <- 0 until inorder.length) {
            pos(inorder(i).index) = i
          }
          val heights = inorder.map(_.height)
          val rmq = new RangeMinQuery(heights)
          for (idx <- possible) {
            val (i, j, w) = edgeData(idx)
            if (i != excluded && j != excluded && !edgeUsed(idx)) {
              //println(rmq(pos(i),pos(j)))
              val idxr = inorder(rmq(pos(i), pos(j))).value // this is the heaviest edge to be removed
              val reducedCost = edgeWeight(idx) - edgeWeight(idxr)
              //println("reducedCost:" + reducedCost)
              if ((oneTreeLBf + reducedCost).ceil.toInt > cost.max) {
                if (edges.excludes(idx) == Failure) return Failure
              }
            }
          }
        }

        
        // update the weights
        val denom: Double = (for (i <- 0 until n) yield ((2 - incident(i)) * (2 - incident(i)))).sum
        var target = if (cost.max - oneTreeLB < 0) oneTreeLB+0.1 else cost.max
        if (denom == 0) stepSize = 0
        else stepSize = alpha * (target - oneTreeLB) / denom
        for (i <- 0 until n) {
          y(i) += (stepSize * (2 - incident(i)))
        }
        
        
        
        
        
        
      }
      // end of iters, can do edge filtering here


      
      
      
      
      alpha *= beta;
      beta /= 2;
      

    }
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
    if (v != idx) {
      edgeVar.excludes(edgeIndex(idx)(v))
    }
    else CPOutcome.Suspend
  }
  
  override def valBindIdx(x: CPIntVar, idx: Int): CPOutcome = {
    edgeVar.requires(edgeIndex(idx)(x.value))
  } 
  
  override def valExcluded(x: CPSetVar, v: Int): CPOutcome = {
    val (i,j,w) = edges(v)
    succ(i-n).removeValue(j)
  }  

  override def valRequired(x: CPSetVar, v: Int): CPOutcome = {
    val (i,j,w) = edges(v)
    if ((i-n) != j) {
      succ(i-n).assign(j)
    } else CPOutcome.Suspend
  }   

}
