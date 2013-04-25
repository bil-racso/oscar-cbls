package oscar.dfo.mogen

import oscar.util.mo.ParetoFront
import oscar.util.mo.MOOPoint
import oscar.util.mo.MOOComparator
import oscar.util.mo.FeasibleRegion
import oscar.dfo.mogen.algos.ComparativeAlgorithm
import oscar.util.mo.LinearList
import oscar.dfo.mogen.algos.ComparativeAlgorithm
import oscar.dfo.mogen.algos.ComparativeAlgorithmState
import oscar.util.mo.RandomGenerator
import scala.reflect.ClassTag
import oscar.util.mo.ArchiveElement

class MOGENTriplet[E <% Ordered[E]](point: MOOPoint[E], algorithm: ComparativeAlgorithm, algorithmState: ComparativeAlgorithmState[E]) extends ArchiveElement[E] {
  /** Returns the MOOPoint contained in the triplet */
  def getMOOPoint: MOOPoint[E] = point
  
  /** Returns the algorithm contained in the triplet */
  def getAlgorithm: ComparativeAlgorithm = algorithm
  
  /** Returns the algorithm contained in the triplet */
  def getAlgorithmState: ComparativeAlgorithmState[E] = algorithmState
  
  /** The number of evaluations contained in the MOOPoint of the triplet */
  def nbEvaluations: Int = point.nbEvaluations
  
  /** The number of coordinates contained in the MOOPoint of the triplet */
  def nbCoordinates: Int = point.nbCoordinates
  
  /** The evaluation at the index referenced by functionIndex contained in the MOOPoint of the triplet */
  def getEvaluation(functionIndex: Int): E = point.getEvaluation(functionIndex)
}

object MOGENTriplet {
  
  def apply[E <% Ordered[E]](point: MOOPoint[E], algorithm: ComparativeAlgorithm, algorithmState: ComparativeAlgorithmState[E]) = new MOGENTriplet(point, algorithm, algorithmState)
}