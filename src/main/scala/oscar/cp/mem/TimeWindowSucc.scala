package oscar.cp.mem

import oscar.cp.core._
import scala.math.max

class TimeWindowSucc(cp: Store, c: Int, succ: Array[CPVarInt], arrival: Array[CPVarInt], dist: Array[Array[Int]], service: Array[Int]) extends Constraint(cp, "TimeWindow") {

  var maxTime = Int.MinValue
  
  override def setup(l: CPPropagStrength): CPOutcome = {

    if (checkAllPred() == CPOutcome.Failure) CPOutcome.Failure
    else if (predPropagate() == CPOutcome.Failure) CPOutcome.Failure
    else  {
      // Predecessors
      if (!succ(c).isBound) succ(c).callValRemoveWhenValueIsRemoved(this)
      // Maximal arrival of this
      if (!arrival(c).isBound) arrival(c).callUpdateMinWhenMinChanges(this)
      // Minimal arrival of predecessors
      for (i <- succ(c)) if (!arrival(i).isBound) arrival(i).callUpdateMaxIdxWhenMaxChanges(this, i)
      
      CPOutcome.Suspend
    }
  }
  
  override def updateMin(cpvar: CPVarInt, v: Int): CPOutcome = checkAllPred

  override def updateMaxIdx(cpvar: CPVarInt, id: Int, v: Int): CPOutcome = {
    if (succ(c) hasValue id) {
      if (checkSucc(id) == CPOutcome.Failure) CPOutcome.Failure
      else predPropagate()
    } else CPOutcome.Suspend
  }

  def checkAllPred() : CPOutcome = {
    for (j <- succ(c)) if (succ(c).exists(j => checkSucc(j) == CPOutcome.Failure))
      return CPOutcome.Failure
    return CPOutcome.Suspend
  }
  
  def checkSucc(j: Int): CPOutcome = {
    if (arrival(c).min + timeDist(c, j) > arrival(j).max) succ(c).removeValue(j)
    else CPOutcome.Suspend
  }
  
  def timeDist(from: Int, to: Int) = service(from) + dist(from)(to)
  
  override def valRemove(cpvar : CPVarInt, id : Int) : CPOutcome = {
    predPropagate()
  }

  def predPropagate(): CPOutcome = {

    updateMaxTime()
    if (arrival(c).updateMax(maxTime) == CPOutcome.Failure) CPOutcome.Failure
    else CPOutcome.Suspend
  }

  def updateMaxTime() {
    maxTime = Int.MinValue
    for (j <- succ(c)) {
      val tauMax = arrival(j).max - timeDist(c, j)
      if (tauMax > maxTime) {
        maxTime = tauMax
      }
    }
  }
}

