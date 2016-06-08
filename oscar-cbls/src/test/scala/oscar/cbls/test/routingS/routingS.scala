package oscar.cbls.test.routingS

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.routing.seq.model.{TotalConstantDistance, VRP, VRPObjective}
import oscar.cbls.routing.seq.neighborhood.{TwoOpt, OnePointMove}
import oscar.cbls.search.combinators.BestSlopeFirst
import oscar.cbls.search.core.EasyNeighborhood

/**
 * Created by rdl on 07-06-16.
 */

class MyRouting(n:Int,v:Int,symmetricDistance:Array[Array[Int]],m:Store)
  extends VRP(n,v,m) with TotalConstantDistance with VRPObjective{

  setSymmetricDistanceMatrix(symmetricDistance)

  addObjectiveTerm(totalDistance)
}

object routingS extends App{

  val n = 1000
  val nodes = 0 until n

  val symmetricDistanceMatrix = RoutingMatrixGenerator(n)._1

  val model = new Store() //checker = Some(new ErrorChecker()))

  val myVRP = new MyRouting(n,1,symmetricDistanceMatrix,model)

  myVRP.setCircuit(nodes)
  model.close()

  val onePtMove = new OnePointMove(() => nodes, ()=>_=>nodes, myVRP)

  val twoOpt = new TwoOpt(() => nodes, ()=>_=>nodes, myVRP)

  val search = BestSlopeFirst(List(onePtMove,twoOpt))
  search.verbose = 1

  search.doAllMoves(obj=myVRP.getObjective())

}


object RoutingMatrixGenerator {

  def apply(N: Int, side: Int = 10000): (Array[Array[Int]],Array[(Int,Int)]) = {

    //we generate te cost distance matrix
    def randomXY: Int = ((math.random * side)).toInt
    val pointPosition: Array[(Int, Int)] = Array.tabulate(N)(w => (randomXY, randomXY))

    def distance(from: (Int, Int), to: (Int, Int)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(N)(
      n1 => Array.tabulate(N)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }
}