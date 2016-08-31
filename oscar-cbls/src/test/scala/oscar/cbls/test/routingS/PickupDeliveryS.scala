package oscar.cbls.test.routingS

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.seq.Size
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.{CascadingObjective, Objective}
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood._
import oscar.cbls.search.combinators._
import oscar.cbls.search.move.CompositeMove

import scala.util.Random

import scala.util.Random

/**
  * Created by fabian on 04-07-16.
  */

class MyPDP(n:Int, v:Int, m:Store,
            symmetricDistance:Array[Array[Int]], maxPivot:Int,
            pickups:Array[Int], deliveries:Array[Int],
            timeWindows:Array[(Int,Int,Int,Int)], ttf:TTFMatrix)
  extends PDP(n,v,m,maxPivot) with ConstantDistancePerVehicle with ClosestNeighbors {

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  val penaltyForUnrouted  = 100000

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  val closestNeighboursForward = computeClosestNeighborsForward()
  def closestNeighboursForwardNotFull = computeClosestNeighborsForwardOneValueFilter(isNotFull())
  def closestNeighboursForwardNotSameRoute = computeClosestNeighborsForward(notOnSameVehicle())
  def closestNeighboursForwardOnSameRoute = computeClosestNeighborsForward(onSameVehicle())

  def size = routes.value.size

  this.addToStringInfo(() => "next:" + next.map(_.value).mkString(","))
  this.addToStringInfo(() => "prev:" + prev.map(_.value).mkString(","))


  addPickupDeliveryCouples(pickups,deliveries)
  setArrivalLeaveLoadValue()
  setVehiclesMaxCargo(5)
  setVehiclesCapacityStrongConstraint()
  setTimeWindows(timeWindows)
  setTravelTimeFunctions(ttf)

  val obj = new CascadingObjective(fastConstraints,
    new CascadingObjective(slowConstraints,
      new CascadingObjective(precedenceObj, Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes)))))))
}

object PickupDeliveryS extends App{
  val n = 310
  val v = 10

  val maxPivotPerValuePercent = 4

  println("PDP(n:" + n + " v:" + v + ")")

  val routingMatrix = RoutingMatrixGenerator(n)

  val symmetricDistanceMatrix = routingMatrix._1

  val model = new Store(noCycle = false)

  val (pickups,deliveries) = RoutingMatrixGenerator.generatePickupDeliveryCouples(n,v)

  val timeWindows = RoutingMatrixGenerator.generateTimeWindows(n, v, pickups, deliveries)

  val ttf = RoutingMatrixGenerator.generateLinearTravelTimeFunction(n,symmetricDistanceMatrix)

  val myPDP = new MyPDP(n,v,model,symmetricDistanceMatrix,maxPivotPerValuePercent,pickups,deliveries,timeWindows,ttf)
  val nodes = myPDP.nodes



  model.close()

  val insertCoupleFast = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=>myPDP.kFirst(n/10,myPDP.closestNeighboursForward,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP, best = true))name "insertCoupleFast")

  val insertCoupleSlow = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myPDP.getUnroutedPickups,
      relevantPredecessor = ()=> myPDP.kFirst(n/5,myPDP.closestNeighboursForward,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP, best = true))name "insertCoupleSlow")

  val insertCoupleCloseToDepot = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => {
        var tempList:List[Int] = List.empty
        for(i <- 0 until v){
          tempList = myPDP.kFirst(n/v,myPDP.closestNeighboursForward,myPDP.isUnroutedPickup)(i).toList ::: tempList
        }
        tempList
      },
      relevantPredecessor = () => myPDP.kFirst(n/10,myPDP.closestNeighboursForward,myPDP.isRouted),
      vrp = myPDP),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
      unroutedNodesToInsert =  () => Iterable(myPDP.getRelatedDelivery(moveResult.insertedPoint)),
      relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP, best = true))name "insertCoupleCloseToDepot")

  val oneCoupleMove = Profile(DynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickups,
    relevantNewPredecessors= () => myPDP.kFirst(20,myPDP.closestNeighboursForwardOnSameRoute,myPDP.isRouted),
    vrp = myPDP),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesToMove = () => List(myPDP.getRelatedDelivery(moveResult.movedPoint)),
      relevantNewPredecessors= () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP, best = true))name "oneCoupleMove")

  val onePointMovePD = Profile(new RoundRobin(List(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickups,
    relevantNewPredecessors = () => myPDP.getNodesBeforeRelatedDelivery(),
    vrp = myPDP),OnePointMove(
    nodesToMove = () => myPDP.getRoutedDeliverys,
    relevantNewPredecessors = () => myPDP.getNodesAfterRelatedPickup(),
    vrp = myPDP)))name "OnePointMove PickupDelivery")

  val pickupDeliveryCoupleShift = Profile(DynAndThen(OnePointMove(
    nodesToMove = () => myPDP.getRoutedPickups,
    relevantNewPredecessors = () => myPDP.kFirst(20,myPDP.closestNeighboursForwardNotSameRoute,myPDP.isRouted),
    vrp = myPDP),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesToMove = () => List(myPDP.getRelatedDelivery(moveResult.movedPoint)),
      relevantNewPredecessors = () => myPDP.getNodesAfterRelatedPickup(),
      vrp = myPDP, best = true))name "pickupDeliveryCoupleShift")

  val removeCouple = Profile(new DynAndThen(new RemovePoint(
    relevantPointsToRemove = () => myPDP.getRoutedPickups,
    vrp = myPDP),
    (moveResult1:RemovePointMove) =>{
      println(moveResult1.pointToRemove)
      println(myPDP.getRelatedDelivery(moveResult1.pointToRemove))
      new RemovePoint(
        relevantPointsToRemove = () => List(myPDP.getRelatedDelivery(moveResult1.pointToRemove)),
        vrp = myPDP)
    }, maximalIntermediaryDegradation = Int.MaxValue)
  )

  def dynAndThenCoupleExchange = {
    var firstVehicle = 10
    var secondVehicle = 10
    Profile(
      new DynAndThen(
        //We first remove a PD couple from a first route
        new DynAndThen(new RemovePoint(
          relevantPointsToRemove = () => {
            myPDP.getRoutedPickups
          },
          vrp = myPDP),
          (moveResult1:RemovePointMove) =>{
            firstVehicle = myPDP.getVehicleOfNode(myPDP.getRelatedDelivery(moveResult1.pointToRemove))
            new RemovePoint(
              relevantPointsToRemove = () => List(myPDP.getRelatedDelivery(moveResult1.pointToRemove)),
              vrp = myPDP)}
        )
        ,(moveResult2:CompositeMove) =>{
          //Then we move a PD couple from a second route to the first route
          new DynAndThen(
            new DynAndThen(OnePointMove(
              nodesToMove = () => myPDP.notOnSameVehicle(myPDP.getRoutedPickups,firstVehicle),
              relevantNewPredecessors = () => (i:Int) => myPDP.getNodesOfVehicle(firstVehicle),
              vrp = myPDP), (moveResult3:OnePointMoveMove) =>{
              secondVehicle = myPDP.getVehicleOfNode(myPDP.getRelatedDelivery(moveResult3.movedPoint))
              OnePointMove(
                nodesToMove = () => List(myPDP.getRelatedDelivery(moveResult3.movedPoint)),
                relevantNewPredecessors= () => myPDP.getNodesAfterRelatedPickup(),
                vrp = myPDP, best = true)}
            )
            ,(moveResult4:CompositeMove) =>{
              //And finally we insert the first couple in the second route
              new DynAndThen(InsertPointUnroutedFirst(
                  unroutedNodesToInsert = () => {
                    Iterable(moveResult2.ml.head.asInstanceOf[RemovePointMove].pointToRemove)
                  },
                  relevantPredecessor = () => (i:Int) => myPDP.getNodesOfVehicle(secondVehicle),
                  vrp = myPDP), (moveResult5:InsertPointMove) =>{
                InsertPointUnroutedFirst(
                  unroutedNodesToInsert = () => Iterable(myPDP.getRelatedDelivery(moveResult5.insertedPoint)),
                  relevantPredecessor = () => myPDP.getNodesAfterRelatedPickup(),
                  vrp = myPDP, best = true)}
              )}
            , maximalIntermediaryDegradation = Int.MaxValue/2
          )}
      )name "dynAndThenCoupleExchange"
    )
  }

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myPDP.kFirst(k,myPDP.closestNeighboursForwardNotFull), myPDP,breakSymmetry = breakSym,neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = BestSlopeFirst(List(insertCoupleCloseToDepot,onePointMovePD)) exhaust
    new BestSlopeFirst(List(pickupDeliveryCoupleShift,oneCoupleMove,insertCoupleSlow,onePointMovePD, threeOpt(20,true)))

  val searchWithRrestart = search onExhaustRestartAfter(Atomic(removeCouple maxMoves((n-v)/2)),5,myPDP.obj)


  search.verbose = 2

  search.paddingLength = 300

  search.doAllMoves(obj=myPDP.obj)

  println(myPDP)
  println(search.profilingStatistics)

}
