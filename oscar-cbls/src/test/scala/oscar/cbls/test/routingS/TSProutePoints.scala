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
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.{CascadingObjective, Objective}
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood.{InsertPointUnroutedFirst, OnePointMove, ThreeOpt, TwoOpt1}
import oscar.cbls.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random


class MySimpleRoutingWithUnroutedPoints(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot)
  with TotalConstantDistance with ClosestNeighbors with RoutedAndUnrouted{

  setSymmetricDistanceMatrix(symmetricDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  //val obj = new CascadingObjective(totalViolationOnRestriction, totalDistance)
  val obj = Objective(totalDistance + (1000000 - Size(routes)*10000))

  this.addToStringInfo(() => "objective: " + obj.value)
  this.addToStringInfo(() => "n:" + n + " v:" + v)

  computeClosestNeighbors()
}

object TSProutePoints extends App{

  val n = 5
  val v = 1

  val maxPivotPerValuePercent = 4

  println("VRP(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  //  println("restrictions:" + restrictions)
  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MySimpleRoutingWithUnroutedPoints(n,v,symmetricDistanceMatrix,model,maxPivotPerValuePercent)
  val nodes = myVRP.nodes

  model.close()

  println(myVRP)

  val routeUnroutdPoint =  Profile(new InsertPointUnroutedFirst(myVRP.unrouted,()=>myVRP.kNearest(100,myVRP.isRouted), myVRP))


  val onePtMove = Profile(new OnePointMove(() => nodes, ()=>myVRP.kNearest(40,myVRP.isRouted), myVRP))

  val twoOpt = Profile(new TwoOpt1(() => nodes, ()=>myVRP.kNearest(40,myVRP.isRouted), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(myVRP.routed, ()=>myVRP.kNearest(k,myVRP.isRouted), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  //val search = BestSlopeFirst(List(onePtMove,twoOpt, threeOpt(10,true))) exhaust threeOpt(20,true)

  val search = routeUnroutdPoint exhaust threeOpt(20,true)

  search.verbose = 1
  search.verboseWithExtraInfo(4,myVRP.toString)
  search.paddingLength = 100

  search.doAllMoves(obj=myVRP.obj)

  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}
