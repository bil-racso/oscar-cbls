package oscar.dfo.multiobjective.evolutionary

import oscar.dfo.utils.MOOPoint
import oscar.algo.paretofront.ParetoElement

class EvolutionaryElement(val point: MOOPoint) extends ParetoElement[Double] {
  def getMOOPoint: MOOPoint = point
  
  def coordinates = point.coordinates
  
  def objectives: Array[Double] = point.objectives
  
  /** The evaluation at the index referenced by functionIndex contained in the MOOPoint of the triplet */
  def getEvaluation(functionIndex: Int): Double = point.getEvaluation(functionIndex)
  
  /** The coordinates of the point */
  def getCoordinates: Array[Double] = point.coordinates
}

object EvolutionaryElement {
  def apply[E](point: MOOPoint) = new EvolutionaryElement(point)
}