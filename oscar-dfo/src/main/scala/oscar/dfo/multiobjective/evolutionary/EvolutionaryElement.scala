package oscar.dfo.multiobjective.evolutionary

import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.ArchiveElement

class EvolutionaryElement[E](val point: MOOPoint[E]) extends ArchiveElement[E] {
  def getMOOPoint: MOOPoint[E] = point
  
  /** The number of evaluations contained in the MOOPoint of the triplet */
  def nbEvaluations: Int = point.nbEvaluations
  
  /** The number of coordinates contained in the MOOPoint of the triplet */
  def nbCoordinates: Int = point.nbCoordinates
  
  /** The evaluation at the index referenced by functionIndex contained in the MOOPoint of the triplet */
  def getEvaluation(functionIndex: Int): E = point.getEvaluation(functionIndex)
}

object EvolutionaryElement {
  def apply[E](point: MOOPoint[E]) = new EvolutionaryElement(point)
}