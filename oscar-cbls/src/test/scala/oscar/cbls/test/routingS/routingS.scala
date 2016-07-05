package oscar.cbls.test.routingS

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.routing.NodeVehicleRestrictions
import oscar.cbls.invariants.lib.seq.{PositionsOf, Size}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.{CascadingObjective, Objective}
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood.{OnePointMove, ThreeOpt, TwoOpt1}
import oscar.cbls.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random


class MyRouting(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int, nodeVehicleRestriction:Iterable[(Int,Int)])
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors with VehicleOfNode{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  //this is useless, but it makes some fun.
  val positionOf48 = PositionsOf(cloneOfRoute, 48)
  this.addToStringInfo(()=>"" + positionOf48)

  val size = Size(cloneOfRoute)
  this.addToStringInfo(()=>"" + size)

  this.addToStringInfo(()=>"number of restrictions:" + nodeVehicleRestriction.size)

  //initializes to something simple
  setCircuit(nodes)

  val violationOfRestriction = NodeVehicleRestrictions(routes,v, nodeVehicleRestriction)

  val totalViolationOnRestriction = Sum(violationOfRestriction)

  //val obj = new CascadingObjective(totalViolationOnRestriction, totalDistance)
  val obj = Objective(totalViolationOnRestriction*10000 + totalDistance)

  this.addToStringInfo(() => "objective: " + obj.value)

  this.addToStringInfo(() => "violationOfRestriction:[" + violationOfRestriction.toList.map(_.value).mkString(",") + "]")

  val nodesThanShouldBeMovedToOtherVehicle = NodeVehicleRestrictions.violatedNodes(vehicleOfNode,v,nodeVehicleRestriction)

  val closestNeighboursForward = computeClosestNeighborsForward()
}

object routingS extends App{

  val n = 1000
  val v = 10
  val nbRestrictions = 3000

  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1
  val restrictions = RoutingMatrixGenerator.generateRestrictions(n,v,nbRestrictions)

  //  println("restrictions:" + restrictions)
  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MyRouting(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent,restrictions)
  val nodes = myVRP.nodes

  model.close()

  println(myVRP)

  val onePtMove = Profile(new OnePointMove(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward), myVRP))

  val onePtMoveSolvingRestrictions = Profile(
    new OnePointMove(myVRP.nodesThanShouldBeMovedToOtherVehicle, ()=>myVRP.kFirst(20,myVRP.closestNeighboursForward), myVRP)
      exhaust OnePointMove(myVRP.nodesThanShouldBeMovedToOtherVehicle, ()=>myVRP.kFirst(100,myVRP.closestNeighboursForward), myVRP)
      guard(() => myVRP.totalViolationOnRestriction.value >0)
      name "MoveForRestr")

  val twoOpt = Profile(new TwoOpt1(() => nodes, ()=>myVRP.kFirst(40,myVRP.closestNeighboursForward), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myVRP.kFirst(k,myVRP.closestNeighboursForward), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = BestSlopeFirst(List(onePtMove,twoOpt, threeOpt(10,true),onePtMoveSolvingRestrictions)) exhaust threeOpt(20,true)

  search.verbose = 1
  //  search.verboseWithExtraInfo(4,()=>myVRP.toString)
  search.paddingLength = 100

  search.doAllMoves(obj=myVRP.obj)

  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}