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
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood.{OnePointMove, TwoOpt}
import oscar.cbls.search.combinators.{BestSlopeFirst, Profile}

import scala.util.Random


class MyRouting(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot) with TotalConstantDistance with VRPObjective
  with ClosestNeighbors  with NodesOfVehicle with VehicleOfNode{

  setSymmetricDistanceMatrix(symmetricDistance)

  addObjectiveTerm(totalDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  computeClosestNeighbors()
}

object routingS extends App{

  val n = 1000
  val v = 1

  val maxPivot = 50

  println("VRP(n:" + n + " v:" + v + ")")

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  val model = new Store()//checker = Some(new ErrorChecker()))

  val myVRP = new MyRouting(n,v,symmetricDistanceMatrix,model,maxPivot)
  val nodes = myVRP.nodes

  myVRP.setCircuit(nodes)
  model.close()

  val onePtMove = Profile(new OnePointMove(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  val twoOpt = Profile(new TwoOpt(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  val search = BestSlopeFirst(List(onePtMove,twoOpt))
  search.verbose = 1//verboseWithExtraInfo(1,()=>myVRP.toString)

  search.doAllMoves(obj=myVRP.getObjective)

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

