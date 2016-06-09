package oscar.cbls.test.routingS

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.core.propagation.ErrorChecker
import oscar.cbls.routing.seq.model._
import oscar.cbls.routing.seq.neighborhood.{OnePointMove, TwoOpt}
import oscar.cbls.search.combinators.{BestSlopeFirst, Profile}


class MyRouting(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store, maxPivot:Int)
  extends VRP(n,v,m,maxPivot) with TotalConstantDistance with VRPObjective with ClosestNeighbors  with NodesOfVehicle with VehicleOfNode{

  setSymmetricDistanceMatrix(symmetricDistance)

  addObjectiveTerm(totalDistance)

  override protected def getDistance(from : Int, to : Int) : Int = symmetricDistance(from)(to)

  computeClosestNeighbors()
}

object routingS extends App{

  val n = 1000
  val v = 1

  val maxPivot = 40

  println("VRP(n:" + n + " v:" + v + ")")

  val nodes = 0 until n

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MyRouting(n,v,symmetricDistanceMatrix,model,maxPivot)

  myVRP.setCircuit(nodes)
  model.close()

  val onePtMove = Profile(new OnePointMove(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  val twoOpt = Profile(new TwoOpt(() => nodes, ()=>myVRP.kNearest(40), myVRP))

  val search = BestSlopeFirst(List(onePtMove,twoOpt))
  search.verbose = 1

  search.doAllMoves(obj=myVRP.getObjective)

  println
  println(myVRP)
  println
  println(search.profilingStatistics)
}


object RoutingMatrixGenerator {

  def apply(N: Int, side: Int = 10000): (Array[Array[Int]],Array[(Int,Int)]) = {

    //we generate te cost distance matrix
    def randomXY: Int = (math.random * side).toInt
    val pointPosition: Array[(Int, Int)] = Array.tabulate(N)(w => (randomXY, randomXY))

    def distance(from: (Int, Int), to: (Int, Int)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(N)(
      n1 => Array.tabulate(N)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }
}