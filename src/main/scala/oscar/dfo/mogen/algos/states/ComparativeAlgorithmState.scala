package oscar.dfo.mogen.algos.states
import oscar.util.mo.MOOPoint
import oscar.util.mo.MOOComparator

trait ComparativeAlgorithmState[E] {
  def getBestPoint: MOOPoint[E]

  def getNewState(newBestPoint: MOOPoint[E], comparator: MOOComparator[E]): ComparativeAlgorithmState[E]

  def getPoints: List[MOOPoint[E]]
}