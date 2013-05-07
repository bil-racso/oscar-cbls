package oscar.dfo.mogen.utils

import oscar.util.mo.MOOPoint
import oscar.util.mo.MOOComparator
import oscar.util.mo.RandomGenerator
import scala.util.continuations._
import oscar.util.mo.MOEvaluator
import oscar.util.mo.FeasibleRegion

trait Simplex[E] {

  val simplex: Array[MOOPoint[E]]
  
  def simplexSize = simplex.length
  
  var bestPoint: MOOPoint[E]

  def getBestPoint = bestPoint
  
  def nbCoordinates = simplex(0).nbCoordinates
  
  def worstPoint = simplex(simplexSize - 1)

  def orderSimplex(comparator: MOOComparator[E]) = {
    simplex.sortWith((point1, point2) => ((point1 == bestPoint) || (comparator.isEquivalent(point1, point2) && 0.5 > RandomGenerator.nextDouble) || comparator.dominates(point1, point2)))
  }

  def getPoints: List[MOOPoint[E]] = simplex.toList
  
  def printSimplex = {
    println("=" * 80)
    for (i <- 0 until simplexSize)
      println(i + ": " + simplex(i).toString)
  }
  
  def getSmallestEdge: Double = {
    val coords = simplex.map(point =>point.coordinates)
    var minDist = Double.MaxValue
    for (i <- 0 until simplex.length) {
      for (j <- (i + 1) until simplex.length) {
        minDist = math.min(minDist, ArrayUtils.euclidianDistance(coords(i), coords(j)))
      }
    }
    minDist
  }
  
  def reinitializeSimplex(intervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): Unit = {
    val newCoords = Array.tabulate(bestPoint.coordinates.length){ index =>
      val randPerturbation = intervals.map(e => (0.5 - RandomGenerator.nextDouble) * math.abs(e._2 - e._1))
      Array.tabulate(bestPoint.coordinates.length)(i => bestPoint.coordinates(i) + randPerturbation(i))
    }
    for (i <- 1 until simplexSize)
      simplex(i) = evaluator.eval(newCoords(i), feasReg)
  }
  
  def onInit(): Unit = {}
  def onReflexion(reflectedPoint: MOOPoint[E]): Unit = {}
  def onExpansion(expandedPoint: MOOPoint[E]): Unit = {}
  def onInsideContraction(expandedPoint: MOOPoint[E]): Unit = {}
  def onOutsideContraction(expandedPoint: MOOPoint[E]): Unit = {}
  def onShrink(expandedPoint: MOOPoint[E]): Unit = {}
}