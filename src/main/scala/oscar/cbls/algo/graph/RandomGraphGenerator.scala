package oscar.cbls.algo.graph

import scala.util.Random

object RandomGraphGenerator {
  def generatePseudoPlanarConditionalGraph(nbNodes : Int,
                                           nbConditionalEdges:Int,
                                           nbNonConditionalEdges:Int,
                                           nbTransitNodes:Int,
                                           mapSide:Int = 1000) : ConditionalGraphWithIntegerNodeCoordinates = {
    //closest edges first
    val totalEdges = nbConditionalEdges + nbNonConditionalEdges

    def randomXY: Int = (math.random * mapSide).toInt
    val pointPosition: Array[(Int, Int)] = Array.tabulate(nbNodes)(w => (randomXY, randomXY))

    val nodes = 0 until nbNodes

    def distance(fromNode: Int,toNode:Int) = {
      val fromCoord = pointPosition(fromNode)
      val tooCoord = pointPosition(toNode)
      2 + math.sqrt(math.pow(fromCoord._1 - tooCoord._1, 2) + math.pow(fromCoord._2 - tooCoord._2, 2)).toInt
    }

    val allDistances = nodes.flatMap(node1 => (0 until node1).map(node2 => (node1,node2,distance(node1,node2))))

    val sortedDistances = allDistances.sortBy(_._3).toList

    val subDistance = sortedDistances.take(totalEdges)
    val scrambled = Random.shuffle(subDistance).iterator

    val isTransitAllowed = Random.shuffle(nodes.toList.map(i => i < nbTransitNodes)).toArray

    val nodeArray = Array.tabulate(nbNodes)(nodeId => new Node(nodeId,isTransitAllowed(nodeId)))

    new ConditionalGraphWithIntegerNodeCoordinates(
      coordinates = pointPosition,
      nodes = nodeArray,
      edges = Array.tabulate(subDistance.size)(edgeId => {
        val edgeData = scrambled.next
        new Edge(
          edgeId,
          nodeA = nodeArray(edgeData._1),
          nodeB = nodeArray(edgeData._2),
          length = edgeData._3,
          conditionID = if(edgeId < nbConditionalEdges) Some(edgeId) else None
        )
      }),
      nbConditions = nbConditionalEdges)
  }
}
