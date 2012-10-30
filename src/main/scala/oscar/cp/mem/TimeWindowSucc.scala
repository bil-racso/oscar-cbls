package oscar.cp.mem

import oscar.cp.core._
import scala.math.max

class TimeWindowSucc(cp: Store, c: Int, succ: Array[CPVarInt], arrival: Array[CPVarInt], dist: Array[Array[Int]], service: Array[Int]) extends Constraint(cp, "TimeWindow") {

  override def setup(l: CPPropagStrength): CPOutcome = {

    val oc = propagate

    if (oc != CPOutcome.Failure) {
      if (!succ(c).isBound) succ(c).callPropagateWhenDomainChanges(this)
      if (!arrival(c).isBound) arrival(c).callPropagateWhenMinChanges(this)
      for (i <- succ(c))
         if (!arrival(i).isBound) arrival(i).callPropagateWhenMaxChanges(this)
    }

    return oc
  }

  def prune(): CPOutcome = {

    for (j <- succ(c)) {
      if (arrival(c).min + service(c) + dist(c)(j) > arrival(j).max) {
        if (succ(c).removeValue(j) == CPOutcome.Failure)
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

    if (succ(c).isBound) CPOutcome.Suspend
    else {
      var maxV = Int.MinValue

      for (j <- succ(c)) {
        val tauMax = arrival(j).max - service(c) - dist(c)(j)

        if (tauMax > maxV)
          maxV = tauMax
      }

      if (arrival(c).updateMax(maxV) == CPOutcome.Failure) CPOutcome.Failure
      else CPOutcome.Suspend
    }
  }
}

