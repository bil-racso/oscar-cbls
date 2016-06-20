package oscar.cbls.test.routingS

import scala.util.Random

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


