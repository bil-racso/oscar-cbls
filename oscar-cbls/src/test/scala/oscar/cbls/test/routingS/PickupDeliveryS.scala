package oscar.cbls.test.routingS

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.seq.Size
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.{CascadingObjective, Objective}
import oscar.cbls.routing.seq.model.{RoutedAndUnrouted, ClosestNeighbors, ConstantDistancePerVehicle, PDP}
import oscar.cbls.routing.seq.neighborhood._
import oscar.cbls.search.combinators.{BestSlopeFirst, RoundRobin, DynAndThen, Profile}

/**
  * Created by fabian on 04-07-16.
  */

class MyPDP(n:Int,v:Int,m:Store,symmetricDistance:Array[Array[Int]],maxPivot:Int,pickups:Array[Int] = null, deliverys:Array[Int] = null)
  extends PDP(n,v,m,maxPivot) with ConstantDistancePerVehicle with ClosestNeighbors with RoutedAndUnrouted{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 100000

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  val closestNeighboursForward = computeClosestNeighborsForward()

  def size = routes.value.size

  this.addToStringInfo(() => "next:" + next.map(_.value).mkString(","))
  this.addToStringInfo(() => "prev:" + prev.map(_.value).mkString(","))

  if(pickups == null || deliverys == null){
    require((n-v)%2 == 0,"You must have a pair number of nodes")
    addPickupDeliveryCouples(Array.tabulate((n-v)/2)(p => p+v), Array.tabulate((n-v)/2)(d => d+v+((n-v)/2)))
  }else{
    addPickupDeliveryCouples(pickups,deliverys)
  }
  setArrivalLeaveLoadValue()
  setVehiclesMaxCargo(5)
  setVehiclesCapacityStrongConstraint()

  val obj = new CascadingObjective(fastConstraints,
    new CascadingObjective(slowConstraints,
      new CascadingObjective(precedenceObj, Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes)))))))
}

object PickupDeliveryS extends App{
  val n = 105
  val v = 5

  val maxPivotPerValuePercent = 4

  println("PDP(n:" + n + " v:" + v + ")")

  val routingMatrix = RoutingMatrixGenerator(n)

  val symmetricDistanceMatrix = routingMatrix._1

  val model = new Store(checker = Some(new ErrorChecker()), noCycle = false)

  val myPDP = new MyPDP(n,v,model,symmetricDistanceMatrix,maxPivotPerValuePercent)
  val nodes = myPDP.nodes

  model.close()


  val insertCouple = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=>myPDP.kFirst(20,myPDP.closestNeighboursForward,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP))name "insertCouple")

  val oneCoupleMove = Profile(DynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickupsPredecessors,
    relevantNewPredecessors= () => myPDP.kFirst(10,myPDP.closestNeighboursForward,myPDP.isRouted),
    vrp = myPDP),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesToMove = () => List(myPDP.prev(myPDP.getRelatedDelivery(moveResult.movedPoint)).value),
      relevantNewPredecessors= () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP, best = true))name "oneCoupleMove")

  val onePointMovePD = Profile(new RoundRobin(List(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickupsPredecessors,
    relevantNewPredecessors = () => myPDP.getNodesBeforeRelatedDelivery(),
    vrp = myPDP,best = true),OnePointMove(
    nodesToMove = () => myPDP.getRoutedDeliverysPredecessors,
    relevantNewPredecessors = () => myPDP.getNodesAfterRelatedPickup(),
    vrp = myPDP, best = true)))name "OnePointMove PickupDelivery")

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myPDP.kFirst(k,myPDP.closestNeighboursForward), myPDP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = insertCouple exhaust new BestSlopeFirst(List(oneCoupleMove,insertCouple,onePointMovePD)) showObjectiveFunction(myPDP.obj)

  search.verbose = 2

  //  search.verboseWithExtraInfo(4,()=>myVRP.toString)
  search.paddingLength = 100

  search.doAllMoves(obj=myPDP.obj)

  search.profilingStatistics

  println(myPDP)
  println(myPDP.obj)
}
