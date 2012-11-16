package oscar.cp.mem

import oscar.cp.core.CPVarInt
import oscar.cp.modeling._
import oscar.util.selectMin

object RoutingUtils {

  def regretHeuristic(cp: CPSolver, succ: Array[CPVarInt], dist: Array[Array[Int]]) = {
    
    while (!allBounds(succ)) {

      var x = -1
      var maxRegret = Int.MinValue

      for (i <- 0 until succ.size; if !succ(i).isBound) {

        var distK1 = Int.MaxValue
        var distK2 = Int.MaxValue

        for (j <- 0 until succ.size; if succ(i) hasValue j) {

          if (dist(i)(j) < distK1) {
            distK2 = distK1
            distK1 = dist(i)(j)
          } else if (dist(i)(j) < distK2) {
            distK2 = dist(i)(j)
          }
        }

        val regret = distK2 - distK1

        if (regret > maxRegret) {
          x = i
          maxRegret = regret
        }
      }

      val v = selectMin(0 until succ.size)(succ(x).hasValue(_))(dist(x)(_)).get

      cp.branch(cp.post(succ(x) == v))(cp.post(succ(x) != v))
    }
  }

  def minDomDistHeuristic(cp: CPSolver, succ: Array[CPVarInt], dist: Array[Array[Int]]) = {
    
    while (!allBounds(succ)) {
      
      val i = selectMin(0 until succ.size)(!succ(_).isBound)(succ(_).size).get
      val j = selectMin(0 until succ.size)(succ(i).hasValue(_))(dist(i)(_)).get

      cp.branch(cp.post(succ(i) == j))(cp.post(succ(i) != j))
    }
  }
  
    def minDomDistHeuristic(cp: CPSolver, pred: Array[CPVarInt], succ: Array[CPVarInt], dist: Array[Array[Int]]) = {
    
    while (!allBounds(succ)) {
      
      val iPred = selectMin(0 until pred.size)(!pred(_).isBound)(pred(_).size).get
      val iSucc = selectMin(0 until succ.size)(!succ(_).isBound)(succ(_).size).get
      
      if (iPred < iSucc) { 
        val j = selectMin(0 until pred.size)(pred(iPred).hasValue(_))(dist(iPred)(_)).get
        cp.branch(cp.post(pred(iPred) == j))(cp.post(pred(iPred) != j))
      }
      else { 
        val j = selectMin(0 until succ.size)(succ(iSucc).hasValue(_))(dist(iSucc)(_)).get
        cp.branch(cp.post(succ(iSucc) == j))(cp.post(succ(iSucc) != j))
      }
    }
  }
}