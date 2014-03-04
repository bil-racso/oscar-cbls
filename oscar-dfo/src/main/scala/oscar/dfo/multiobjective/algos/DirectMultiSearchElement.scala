package oscar.dfo.multiobjective.algos

import oscar.algo.paretofront.ParetoElement
import oscar.dfo.multiobjective.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.multiobjective.mogen.algos.ComparativeAlgorithm
import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.Utils
import sun.org.mozilla.javascript.internal.Evaluator
import oscar.dfo.utils.MOEvaluator

class DirectMultiSearchElement(point: MOOPoint, var alpha: Double) extends ParetoElement[Double] {
  /** The coordinates of the point */
  def coordinates: Array[Double] = point.coordinates
  
  /** The objectives of the point */
  def objectives: Array[Double] = point.evaluations
  
  /** Returns the MOOPoint contained in the element */
  def getMOOPoint: MOOPoint = point
  
  /** Returns the step size parameter */
  def getAlpha: Double = alpha
  
  /** The evaluation at the index referenced by functionIndex contained in the MOOPoint of the element */
  def getEvaluation(functionIndex: Int): Double = point.getEvaluation(functionIndex)
  
  /** Returns the points found by the algorithm */
  def getPollCoordinates(basis: Array[Array[Double]]): Array[Array[Double]] = {
    Array.tabulate(basis.length)(i => {
      Array.tabulate(nCoordinates)(j => coordinates(j) + alpha * basis(i)(j))
    })
  }
}

object DirectMultiSearchElement {
  def apply(point: MOOPoint, alpha: Double) = new DirectMultiSearchElement(point, alpha)
}