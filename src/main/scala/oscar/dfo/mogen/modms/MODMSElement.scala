package oscar.dfo.mogen.modms

import oscar.util.mo.MOOPoint
import oscar.util.mo.ArchiveElement

class MODMSElement[E <% Ordered[E]](val point: MOOPoint[E], var alpha: Double) extends ArchiveElement[E] {
  def getMOOPoint: MOOPoint[E] = point
  
  /** The number of evaluations contained in the MOOPoint of the triplet */
  def nbEvaluations: Int = point.nbEvaluations
  
  /** The number of coordinates contained in the MOOPoint of the triplet */
  def nbCoordinates: Int = point.nbCoordinates
  
  /** The evaluation at the index referenced by functionIndex contained in the MOOPoint of the triplet */
  def getEvaluation(functionIndex: Int): E = point.getEvaluation(functionIndex)

  /** The value of the alpha for this point */
  def getAlpha: Double = alpha

  /** Changes the value of alpha, multiplying it by a constant */
  def updateAlpha(factor: Double) = alpha *= factor
}

object MODMSElement {
  def apply[E <% Ordered[E]](point: MOOPoint[E], alpha: Double) = new MODMSElement(point, alpha)
}