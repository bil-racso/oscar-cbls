package oscar.cbls.test.routingS

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.seq.Size
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.{CascadingObjective, Objective}
import oscar.cbls.routing.seq.model.{RoutedAndUnrouted, ClosestNeighbors, ConstantDistancePerVehicle, PDP}
import oscar.cbls.routing.seq.neighborhood.{OnePointMoveMove, OnePointMove, InsertPointMove, InsertPointUnroutedFirst}
import oscar.cbls.search.combinators.{DynAndThen, Profile}

/**
  * Created by fabian on 04-07-16.
  */

class MyPDP(n:Int,v:Int,m:Store,symmetricDistance:Array[Array[Int]],maxPivot:Int)
  extends PDP(n,v,m,maxPivot) with ConstantDistancePerVehicle with ClosestNeighbors with RoutedAndUnrouted{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 10000

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size

  this.addToStringInfo(() => "next:" + next.map(_.value).mkString(","))
  this.addToStringInfo(() => "prev:" + prev.map(_.value).mkString(","))

  addPickupDeliveryCouples(Array.tabulate((n-v)/2)(p => p+v), Array.tabulate((n-v)/2)(d => d+v+((n-v)/2)))
  setArrivalLeaveLoadValue()
  setVehiclesMaxCargo(5)
  setVehiclesCapacityStrongConstraint()

  val obj = new CascadingObjective(fastConstraints,
    new CascadingObjective(slowConstraints,
      new CascadingObjective(precedenceObj, Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes)))))))
}

object PickupDeliveryS extends App{
  val n = 20
  val v = 4

  val maxPivotPerValuePercent = 4

  println("PDP(n:" + n + " v:" + v + ")")

  val routingMatrix = RoutingMatrixGenerator(n)

  val symmetricDistanceMatrix = routingMatrix._1

  val model = new Store(checker = Some(new ErrorChecker()), noCycle = false)

  val myPDP = new MyPDP(n,v,model,symmetricDistanceMatrix,maxPivotPerValuePercent)
  val nodes = myPDP.nodes

  model.close()

  println(myPDP)



  val insertCouple = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=>myPDP.kFirst(10,myPDP.closestNeighboursForward,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterPosition(isNodeInserted = false,moveResult.insertAtPosition),
      vrp = myPDP))name "insertCouple")

  val oneCoupleMove = Profile(DynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickupsPredecessors,
    relevantNewPredecessors= () => myPDP.kFirst(10,myPDP.closestNeighboursForward,myPDP.isRouted),
    vrp = myPDP),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesToMove = () => List(myPDP.prev(myPDP.getRelatedDelivery(moveResult.movedPoint)).value),
      relevantNewPredecessors= () => myPDP.getNodesAfterPosition(pos = moveResult.movedPointPosition),
      vrp = myPDP, best = true))name "oneCoupleMove")

  val search = insertCouple exhaust oneCoupleMove

  search.verbose = 4
  //  search.verboseWithExtraInfo(4,()=>myVRP.toString)
  search.paddingLength = 100

  search.doAllMoves(obj=myPDP.obj)

  println(myPDP)
  println(myPDP.obj)
}
