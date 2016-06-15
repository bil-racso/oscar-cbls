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

import oscar.cbls.invariants.core.computation.{SeqValue, Store}
import oscar.cbls.invariants.lib.seq.{Size, PositionsOfConst}
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood.{OnePointMove, ThreeOpt, TwoOpt1}
import oscar.cbls.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random


class MyRouting(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot) with TotalConstantDistance with VRPObjective
  with ClosestNeighbors with VehicleOfNode with NodesOfVehicle {

  setSymmetricDistanceMatrix(symmetricDistance)

  addObjectiveTerm(totalDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  //this is useless, but it makes some fun.
  val positionOf48 = PositionsOfConst(cloneOfRoute, 48)
  this.addToStringInfo(()=>"" + positionOf48)

  val size = Size(cloneOfRoute)
  this.addToStringInfo(()=>"" + size)

  computeClosestNeighbors()
}

object routingS extends App{

  val n = 1000
  val v = 11

  val maxPivot = 40

  println("VRP(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  val model = new Store()//checker = Some(new ErrorChecker()))

  val myVRP = new MyRouting(n,v,symmetricDistanceMatrix,model,maxPivot)
  val nodes = myVRP.nodes

  myVRP.setCircuit(nodes)
  model.close()

  val onePtMove = Profile(new OnePointMove(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  val twoOpt = Profile(new TwoOpt1(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  def threeOpt(k:Int, breakSym:Boolean) = Profile(new ThreeOpt(() => nodes, ()=>myVRP.kNearest(k), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val search = BestSlopeFirst(List(onePtMove,twoOpt, threeOpt(10,false))) exhaust threeOpt(20,true) //afterMove model.propagate() //exhaust threeOpt(40,true)

//  val search = threeOpt(10,false)// afterMove model.propagate() //exhaust threeOpt(40,true)

  search.verbose = 1
  //search.verboseWithExtraInfo(1,()=>myVRP.toString)
  search.paddingLength = 200

  search.doAllMoves(obj=myVRP.getObjective)

  model.propagate()

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}

object RoutingMatrixGenerator {

  def apply(N: Int, side: Int = 10000): (Array[Array[Int]],Array[(Int,Int)]) = {

    val random = new Random(0)
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
}

