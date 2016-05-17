package oscar.examples.cbls.pdptw

/**
  * Created by fabian on 28-04-16.
  */
object PickupDeliveryStarMatrixGenerator {

  def apply(N: Int,V: Int, side: Int = 10000): (Array[Array[Int]],Array[(Int,Int)]) = {

    //we generate te cost distance matrix
    def randomXY: Int = ((math.random * side)).toInt
    val pointPosition: Array[(Int, Int)] = Array.tabulate(N)(w => (randomXY, randomXY))

    for(i <- V until ((N-V)/2 + V)){
      pointPosition(i) = (side/2,side/2)
    }

    def distance(from: (Int, Int), to: (Int, Int)) =
      math.sqrt(math.pow(from._1 - to._1, 2) + math.pow(from._2 - to._2, 2)).toInt

    //for each delivery point, the distance to each warehouse
    (Array.tabulate(N)(
      n1 => Array.tabulate(N)(
        n2 => distance(pointPosition(n1), pointPosition(n2)))),pointPosition)
  }

}
