package oscar.dfo.mogen.algos.states

import oscar.util.mo.MOOPoint
import oscar.util.mo.MOOComparator
import oscar.util.mo.RandomGenerator

trait Simplex[E] {

  val simplex: Array[MOOPoint[E]]
  
  val simplexSize = simplex.length
  
  var bestPoint = simplex(0)

  def getBestPoint = bestPoint
  
  def nbCoordinates = simplex(0).nbCoordinates
  
  def worstPoint = simplex(simplexSize - 1)

  def orderSimplex(comparator: MOOComparator[E]) = {
    simplex.sortWith((point1, point2) => ((point1 == bestPoint) || (comparator.isEquivalent(point1, point2) && 0.5 > RandomGenerator.nextDouble) || comparator.dominates(point1, point2)))
  }

  def getPoints: List[MOOPoint[E]] = simplex.toList

  def arraySum(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) + ar2(i))
  
  def arrayDiff(ar1: Array[Double], ar2: Array[Double]): Array[Double] = Array.tabulate(ar1.length)(i => ar1(i) - ar2(i))
  
  def arrayProd(ar: Array[Double], factor: Double): Array[Double] = Array.tabulate(ar.length)(i => factor * ar(i))
}