package oscar.cbls.test.routingS

import oscar.cbls.routing.seq.model.{TTFConst, TTFMatrix, PDP}

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

  def generatePrecedence(n:Int,v:Int,nbPRecedences:Int):List[(Int,Int)] = {
    val allNodes = v until n

    val randomizedNodes = random.shuffle(allNodes.toList).toIterator
    var precedencesToGenerate = nbPRecedences
    var toReturn:List[(Int,Int)] = List.empty

    while(precedencesToGenerate > 0){
      precedencesToGenerate -= 1
      toReturn = (randomizedNodes.next(),randomizedNodes.next()) :: toReturn
    }

    toReturn
  }

  def generateTimeWindows(n:Int, v:Int, pickups:Array[Int], deliveries:Array[Int], timeUnitDelta:Int = 50): Array[(Int,(Int,Int))] ={

    val maxNodes = n-v
    val maxCouples = maxNodes/2
    val random = new Random(0)
    val timeWindows = Array.tabulate(n)(tw => (0,(0,0)))
    val nbOfNodesPerTimeUnit = Array.tabulate(n*timeUnitDelta)(i => 0)
    var tWGenerated = 0
    var tWCoupleGenerated = 0
    var currentTimeUnit = 0

    def currentTWGenerated(): Int ={
      var res = 0
      var i = 0
      while(i < currentTimeUnit) {
        res += nbOfNodesPerTimeUnit(i)
        i += 1
      }
      res
    }


    while(tWCoupleGenerated < maxCouples){
      val nbOfCouplesToAdd = Math.min(maxCouples - tWCoupleGenerated,random.nextInt(v))
      for(inc <- 0 until nbOfCouplesToAdd){
        val incDelivery = random.nextInt(50)
        timeWindows(pickups(tWCoupleGenerated+inc)) = (timeUnitDelta*(currentTimeUnit+1),(0,timeUnitDelta*currentTimeUnit))
        timeWindows(deliveries(tWCoupleGenerated+inc)) = (timeUnitDelta*(currentTimeUnit+1+incDelivery),(0, timeUnitDelta*(currentTimeUnit+incDelivery)))
        nbOfNodesPerTimeUnit(currentTimeUnit+incDelivery) += 1
      }
      nbOfNodesPerTimeUnit(currentTimeUnit) += nbOfCouplesToAdd
      tWGenerated = currentTWGenerated()
      tWCoupleGenerated += nbOfCouplesToAdd
      currentTimeUnit += random.nextInt(10000/timeUnitDelta)
    }
    timeWindows
  }

  def generatePickupDeliveryCouples(n:Int, v:Int): (Array[Int],Array[Int]) ={
    require((n-v)%2 == 0,"You must have a pair number of nodes")
    (Array.tabulate((n-v)/2)(p => p+v), Array.tabulate((n-v)/2)(d => d+v+((n-v)/2)))
  }

  def generateLinearTravelTimeFunction(n:Int,distanceMatrix: Array[Array[Int]]): TTFMatrix = {
    val ttf = new TTFMatrix(n, new TTFConst(500))
    for (i <- 0 until n)
      for (j <- 0 until n)
        ttf.setTTF(i, j, new TTFConst(distanceMatrix(i)(j)))
    ttf
  }
}


