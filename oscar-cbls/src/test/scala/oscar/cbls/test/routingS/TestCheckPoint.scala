package oscar.cbls.test.routingS

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.invariants.lib.routing.RouteSuccessorAndPredecessors
import oscar.cbls.invariants.lib.seq.Size
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.Objective
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood._
import oscar.cbls.search.combinators.{BestSlopeFirst, DynAndThen, Profile}

/**
  * Created by f.germeau on 07/09/2016.
  */
class MyVRP(n:Int,v:Int,model:Store,symmetricDistanceMatrix:Array[Array[Int]]) extends VRP(n,v,model)
  with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted with NextAndPrev{

  setSymmetricDistanceMatrix(symmetricDistanceMatrix)

  override protected def getDistance(from: Int, to: Int): Int = symmetricDistanceMatrix(from)(to)

  val penaltyForUnrouted = 10000

  val obj = Objective(totalDistance + (penaltyForUnrouted*(n - Size(routes))))

  val closestNeighborsForward = computeClosestNeighborsForward()
}

object TestCheckPoint extends App{

  val n = 500
  val v = 10
  val maxMovesBeforeRollBack = 100

  println("VRP(n:" + n + " v:" + v + ")")

  val (symmetricDistanceMatrix,pointsList) = RoutingMatrixGenerator(n)

  val model = Store(noCycle = false, checker = Some(new ErrorChecker))

  val myVRP = new MyVRP(n,v,model,symmetricDistanceMatrix)

  model.close()

  val insertPoint = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => myVRP.unroutedNodes,
    relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val onePointMove = Profile(OnePointMove(
    nodesToMove = myVRP.routed,
    relevantNewPredecessors = () => myVRP.kFirst(n/10, myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val twoOpt = Profile(TwoOpt1(
    segmentStartValues = myVRP.routed,
    relevantNewSuccessors = () => myVRP.kFirst(n/10, myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val threeOpt = Profile(ThreeOpt(
    potentialInsertionPoints = myVRP.routed,
    relevantNeighbors = () => myVRP.kFirst(20, myVRP.closestNeighborsForward,myVRP.isRouted),
    vrp = myVRP
  ))

  val doubleInsert = Profile(DynAndThen(
    InsertPointUnroutedFirst(
      unroutedNodesToInsert = () => myVRP.unroutedNodes,
      relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward,myVRP.isRouted),
      vrp= myVRP
    ), (moveResult:InsertPointMove)=>
      InsertPointUnroutedFirst(
        unroutedNodesToInsert = () => myVRP.unroutedNodes,
        relevantPredecessor = () => myVRP.kFirst(n/10,myVRP.closestNeighborsForward,myVRP.isRouted),
        vrp= myVRP
      )
  ))

  var checkPoint = myVRP.routes.defineCurrentValueAsCheckpoint(true)
  var movesBeforeRollBack = 1
  var moves = 0

  val search = Profile(BestSlopeFirst(List(insertPoint,onePointMove,twoOpt,threeOpt,doubleInsert))).afterMove({
    if(moves == movesBeforeRollBack && movesBeforeRollBack < maxMovesBeforeRollBack) {
      myVRP.routes.rollbackToCurrentCheckpoint(checkPoint)
      println("rolled back")
      movesBeforeRollBack += 1
      moves = 0
    }
    moves += 1
  })

  search.verbose = 1
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)
  search.paddingLength = 100

  print("Doing all moves ...")


  search.doAllMoves(obj = myVRP.obj)
  model.propagate()
  println(search.profilingStatistics)
}
