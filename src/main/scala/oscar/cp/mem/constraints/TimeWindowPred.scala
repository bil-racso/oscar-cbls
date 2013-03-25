package oscar.cp.mem.constraints

import oscar.cp.core._

class TimeWindowPred(cp: Store, c: Int, pred: Array[CPVarInt], arrival: Array[CPVarInt], dist: Array[Array[Int]], service: Array[Int]) extends Constraint(cp, "TimeWindow") {

  var minPred = -1
  var minTime = Int.MaxValue

  override def setup(l: CPPropagStrength): CPOutcome = {
		  
    if (checkAllPred() == CPOutcome.Failure) CPOutcome.Failure
    else if (predPropagate() == CPOutcome.Failure) CPOutcome.Failure
    else  {
      // Predecessors
      if (!pred(c).isBound) pred(c).callValRemoveWhenValueIsRemoved(this)
      // Maximal arrival of this
      if (!arrival(c).isBound) arrival(c).callUpdateMaxWhenMaxChanges(this)
      // Minimal arrival of predecessors
      for (i <- pred(c)) if (!arrival(i).isBound) arrival(i).callUpdateMinIdxWhenMinChanges(this, i)
      
      CPOutcome.Suspend
    }
  }

  override def updateMax(cpvar: CPVarInt, v: Int): CPOutcome = checkAllPred

  override def updateMinIdx(cpvar: CPVarInt, id: Int, v: Int): CPOutcome = {
    if (pred(c) hasValue id) {
      if (checkPred(id) == CPOutcome.Failure) CPOutcome.Failure
      else predPropagate()
    } else CPOutcome.Suspend
  }

  def checkAllPred() : CPOutcome = {
    for (j <- pred(c)) if (pred(c).exists(j => checkPred(j) == CPOutcome.Failure))
      return CPOutcome.Failure
    return CPOutcome.Suspend
  }

  def checkPred(j: Int): CPOutcome = {
    if (arrival(j).min + timeDist(j, c) > arrival(c).max) pred(c).removeValue(j)
    else CPOutcome.Suspend
  }

  def timeDist(from: Int, to: Int) = service(from) + dist(from)(to)
  
  override def valRemove(cpvar : CPVarInt, id : Int) : CPOutcome = {
    predPropagate()
  }

  def predPropagate(): CPOutcome = {

    updateMinTime()
    if (arrival(c).updateMin(minTime) == CPOutcome.Failure) CPOutcome.Failure
    else CPOutcome.Suspend
  }

  def updateMinTime() {
    minTime = Int.MaxValue
    for (j <- pred(c)) {
      val tauMin = arrival(j).min + timeDist(j, c)
      if (tauMin < minTime) {
        minTime = tauMin
        minPred = j
      }
    }
  }
}

