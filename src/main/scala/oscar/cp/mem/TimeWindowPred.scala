package oscar.cp.mem

import oscar.cp.core._
import scala.math.max

class TimeWindowPred(cp: Store, c: Int, pred: Array[CPVarInt], arrival: Array[CPVarInt], dist: Array[Array[Int]], service: Array[Int]) extends Constraint(cp, "TimeWindow") {

  override def setup(l: CPPropagStrength): CPOutcome = {

    val oc = propagate

    if (oc != CPOutcome.Failure) {
      if (!pred(c).isBound) pred(c).callPropagateWhenDomainChanges(this)
      if (!arrival(c).isBound) arrival(c).callPropagateWhenMaxChanges(this)
      for (i <- pred(c)) {
         if (!arrival(i).isBound) arrival(i).callPropagateWhenMinChanges(this)
      }
    }

    return oc
  }

  def prune(): CPOutcome = {

    for (j <- pred(c)) {
      if (arrival(j).min + service(j) + dist(j)(c) > arrival(c).max) {
        if (pred(c).removeValue(j) == CPOutcome.Failure)
          return CPOutcome.Failure
      }
    }

    return CPOutcome.Suspend
  }

  override def propagate(): CPOutcome = {

    if (prune == CPOutcome.Failure) CPOutcome.Failure
    else if (predPropagate(c) == CPOutcome.Failure) CPOutcome.Failure
    else CPOutcome.Suspend
  }

  def predPropagate(c: Int): CPOutcome = {

    var minV = Int.MaxValue

    for (j <- pred(c)) {
      val tauMin = arrival(j).min + service(j) + dist(j)(c)

      if (tauMin < minV)
        minV = tauMin
    }

    if (arrival(c).updateMin(minV) == CPOutcome.Failure) CPOutcome.Failure
    else CPOutcome.Suspend
  }
  
  def adjustMin: CPOutcome = {
    null
  }
}

