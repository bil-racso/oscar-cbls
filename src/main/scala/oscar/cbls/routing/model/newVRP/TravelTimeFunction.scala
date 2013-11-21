package oscar.cbls.routing.model.newVRP

import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.invariants.lib.logic.IntVarIntVar2IntVarFun
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.constraints.lib.basic.{GE, LE}
import collection.immutable.SortedMap
import oscar.cbls.invariants.core.algo.heap.BinomialHeap
import math._
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.constraints.lib.basic.GE
import oscar.cbls.constraints.lib.basic.LE

abstract class ttfs{
  def getTravelDuration(from:Int, leaveTime:Int,to:Int):Int
  def getMinMaxTravelDuration(from:Int,to:Int):(Int,Int)
}

trait Time extends VRP{
  val ArrivalTime = Array.tabulate(N) {(i:Int) => IntVar(m, 0, Int.MaxValue / N, 0, "arrivalTimeAtNode" + i)}
  val LeaveTime = Array.tabulate(N) {(i:Int) => IntVar(m, 0, Int.MaxValue / N, 0, "leaveTimeAtNode" + i)}

  def setFixedDurationNode(node:Int,duration:Int){
    LeaveTime(node) <== ArrivalTime(node) + duration
  }

  def setFixedDurationNode(node:Int, duration:Int, startWindow:Int){
    LeaveTime(node) <== Max2(ArrivalTime(node),startWindow) + duration
  }
}

/** when the cost of a hop is more complex than a distance matrix.
  * Beware, you must still define the leaveTime from the ArrivalTime (or not)
  * and you can post strong constraints on these values
  */
trait TravelTimeFunction extends VRP with Predecessors with Time{

  protected var travelCosts:ttfs = null

  /** sets the cost function
    * @param travelCosts
    */
  def setTravelTimeFunctions(travelCosts:ttfs){
    for (i <- 0 to N-1){
      ArrivalTime(i) <== new IntVarIntVar2IntVarFun(LeaveTime.element(preds(i)), preds(i),
        (leaveTime,predecessor) =>  if (predecessor == N) 0 else travelCosts.getTravelDuration(predecessor,leaveTime,i) + leaveTime)
    }
  }
}

trait TimeWindow extends Time with StrongConstraints{
  def setEndWindow(node:Int,endWindow:Int){
    strongConstraints.post(LE(LeaveTime(node),endWindow))
  }

  def setFixedDurationNode(node:Int, duration:Int, startWindow:Int, maxWaiting:Int){
    setFixedDurationNode(node, duration, startWindow)
    strongConstraints.post(GE(ArrivalTime(node),startWindow - maxWaiting))
  }
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
 */
trait TimeClosesNeighborPoints extends ClosestNeighborPoints with TravelTimeFunction{
  def getDistance(from: Int, to: Int) {
    travelCosts.getMinMaxTravelDuration(from,to)._1
  }
}
