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

  computeClosestNeighbors()
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

  val onePtMove = Profile(new OnePointMove(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  val onePtMoveSolvingRestrictions = Profile(
    new OnePointMove(myVRP.nodesThanShouldBeMovedToOtherVehicle, ()=>myVRP.kNearest(20), myVRP)
      exhaust OnePointMove(myVRP.nodesThanShouldBeMovedToOtherVehicle, ()=>myVRP.kNearest(100), myVRP)
      guard(() => myVRP.totalViolationOnRestriction.value >0)
      name "MoveForRestr")

  val twoOpt = Profile(new TwoOpt1(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myVRP.kNearest(k), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

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

object RoutingMatrixGenerator {
  val random = new Random(0)

  def apply(N: Int, side: Int = 10000): (Array[Array[Int]],Array[(Int,Int)]) = {

    //we generate te cost distance matrix
    def randomXY: Int = (random.nextFloat() * side).toInt
    val pointPosition: Array[(Int, Int)] = Array.tabulate(N)(w => (randomXY, randomXY))

    def distance(from: (Int, Int), to: (Int, Int)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(N)(
      n1 => Array.tabulate(N)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }

  def generateRestrictions(n:Int,v:Int,nbRestrictions:Int):Iterable[(Int,Int)] = {
    var toReturn = List.empty[(Int,Int)]

    var toGenerate = nbRestrictions
    while(toGenerate !=0){
      val vehicle = (random.nextFloat()*(v)).toInt
      val node = ((random.nextFloat()*(n-v))+v).toInt
      toReturn = (node,vehicle) :: toReturn
      toGenerate -= 1
    }
    toReturn
  }
}


