package oscar.cbls.test.routingS

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.lib.seq.Size
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.{CascadingObjective, Objective}
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood._
import oscar.cbls.search.combinators.{BestSlopeFirst, RoundRobin, DynAndThen, Profile}

/**
  * Created by fabian on 04-07-16.
  */

class MyPDP(n:Int,v:Int,m:Store,symmetricDistance:Array[Array[Int]],maxPivot:Int,pickups:Array[Int] = null, deliverys:Array[Int] = null)
  extends PDP(n,v,m,maxPivot) with ConstantDistancePerVehicle with ClosestNeighbors with RoutedAndUnrouted with TimeWindow with TravelTimeAsFunction{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  def setLinearTravelTimeFunction(distanceMatrix: Array[Array[Int]]): Unit ={
    val ttf = new TTFMatrix(n,new TTFConst(500))
    for(i <- 0 until n){
      for(j <- 0 until n){
        ttf.setTTF(i,j,new TTFConst(distanceMatrix(i)(j)))
      }
    }
    setTravelTimeFunctions(ttf)
  }

  def endWindowGenerator(): Unit ={
    val currentArray:Array[Int] = new Array[Int](n-v)
    val randomIncValues:List[Int] = 2::3::4::5::Nil
    val currentSum = (pos:Int) => {
      var res = 0
      for(i <- 0 to pos) res += currentArray(i)
      res
    }

    val currentPickup:Array[Int] = new Array[Int](n-v)
    val currentSumPickup = (pos:Int) => {
      var res = 0
      for(i <- 0 to pos) res += currentPickup(i)
      res
    }
    var currentTimeUnit = 1
    val nodesOrderedByType = getPickups.toArray
    while(currentSum(currentTimeUnit) < n-v){
      val current = currentSum(currentTimeUnit)
      val currentPick = currentSumPickup(currentTimeUnit)
      val nbOfNodeToAdd = if(n - v - (n-v)/2 - currentPick < v) n-currentPick - (n-v)/2 -v else Math.min(n-v - (n-v)/2 -currentPick,Math.random()*(v*currentTimeUnit - current)).toInt
      for(inc <- 0 until nbOfNodeToAdd){
        val deliveryInc = randomIncValues(scala.util.Random.nextInt(4))
        setEndWindow(nodesOrderedByType(currentPick+inc), 500*(currentTimeUnit+1), slowConstraints)
        setNodeDuration(nodesOrderedByType(currentPick+inc),50,500*currentTimeUnit)
        setEndWindow(getRelatedDelivery(nodesOrderedByType(currentPick+inc)),500*(currentTimeUnit+5),slowConstraints)
        setNodeDuration(getRelatedDelivery(nodesOrderedByType(currentPick+inc)), 0, 500*(currentTimeUnit+deliveryInc))
        currentArray(currentTimeUnit+deliveryInc) += 1
      }
      currentArray(currentTimeUnit) += nbOfNodeToAdd
      currentPickup(currentTimeUnit) += nbOfNodeToAdd
      currentTimeUnit += 1
    }
  }

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
  endWindowGenerator()
  setLinearTravelTimeFunction(distanceMatrix)

  val obj = new CascadingObjective(fastConstraints,
    new CascadingObjective(slowConstraints,
      new CascadingObjective(precedenceObj, Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes)))))))
}

object PickupDeliveryS extends App{
  val n = 410
  val v = 10

  val maxPivotPerValuePercent = 4

  println("PDP(n:" + n + " v:" + v + ")")

  val routingMatrix = RoutingMatrixGenerator(n)

  val symmetricDistanceMatrix = routingMatrix._1

  val model = new Store(noCycle = false)

  val myPDP = new MyPDP(n,v,model,symmetricDistanceMatrix,maxPivotPerValuePercent)
  val nodes = myPDP.nodes

  model.close()


  val insertCoupleFast = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=>myPDP.kFirst(5,myPDP.closestNeighboursForward,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP))name "insertCoupleFast")

  val insertCoupleSlow = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=>myPDP.kFirst(n/10,myPDP.closestNeighboursForward,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP))name "insertCoupleSlow")

  val oneCoupleMove = Profile(DynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickupsPredecessors,
    relevantNewPredecessors= () => myPDP.kFirst(20,myPDP.closestNeighboursForward,myPDP.isRouted),
    vrp = myPDP, best = true),
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

  val pickupDeliveryCoupleShift = Profile(DynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickupsPredecessors,
    relevantNewPredecessors = () => myPDP.kFirst(n/10,myPDP.closestNeighboursForward,myPDP.isRouted),
    vrp = myPDP),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesToMove = () => List(myPDP.prev(myPDP.getRelatedDelivery(moveResult.movedPoint)).value),
      relevantNewPredecessors = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP, best = true))name "pickupDeliveryCoupleShift")

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myPDP.kFirst(k,myPDP.closestNeighboursForward), myPDP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = insertCoupleFast exhaust new BestSlopeFirst(List(pickupDeliveryCoupleShift,oneCoupleMove,insertCoupleSlow,onePointMovePD))

  for(d <- symmetricDistanceMatrix)
    println(d.toList)
  println(myPDP)
  search.verbose = 2

  //  search.verboseWithExtraInfo(4,()=>myVRP.toString)
  search.paddingLength = 300

  search.doAllMoves(obj=myPDP.obj)

  println(myPDP)
  println(search.profilingStatistics)
}
