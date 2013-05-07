package oscar.dfo.mogen.algos

import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.MOEvaluator
import oscar.dfo.mogen.algos.states.ComparativeAlgorithmState
import oscar.util.mo.ParetoFront
import oscar.util.mo.FeasibleRegion
import oscar.dfo.mogen.algos.states.NelderMeadState
import oscar.dfo.mogen.algos.states.MultiDirectionalSearchState

object MultiDirectionalSearch extends ComparativeAlgorithm {
  
  var tolerance = math.pow(10.0, -3.0)
  
  def singleIteration[E](state: ComparativeAlgorithmState[E], currentArchive: ParetoFront[E], feasReg: FeasibleRegion, comparator: MOOComparator[E], evaluator: MOEvaluator[E]): List[MOOPoint[E]] = {
    state match {
      case mdsState: MultiDirectionalSearchState[E] => {
        println("Smallest simplex edge: " + mdsState.getSmallestEdge)
        mdsState.printSimplex
        val rotatedPoints = mdsState.getRotation(evaluator, feasReg)
        val goodRotatedPoints = rotatedPoints.drop(1).filter(candidatePoint => comparator.cmpWithArchive(candidatePoint, mdsState.bestPoint, currentArchive))
        // The rotation discovered some new interesting points
        if (goodRotatedPoints.length > 0) {
          // If rotated points are even better than former best point, an expansion is performed
          if (goodRotatedPoints.filter(candidatePoint => !comparator.cmpWithArchive(mdsState.bestPoint, candidatePoint, currentArchive)).length > 0) {
            val expandedPoints = mdsState.getExpansion(evaluator, feasReg)
            val rotationScore = rotatedPoints.drop(1).foldLeft(0)((acc, newPoint) => acc + currentArchive.score(newPoint, comparator.dominates))
            val expansionScore = expandedPoints.drop(1).foldLeft(0)((acc, newPoint) => acc + currentArchive.score(newPoint, comparator.dominates))
            // Expanded points are more interesting than rotated points
            if (expansionScore >= rotationScore) {
              mdsState.applyMultiPointTransformation(expandedPoints, comparator)
              expandedPoints.drop(1).toList
            }
            // Rotated points are more interesting than expanded points
            else {
              mdsState.applyMultiPointTransformation(rotatedPoints, comparator)
              rotatedPoints.drop(1).toList
            }
          }
          else {
            mdsState.applyMultiPointTransformation(rotatedPoints, comparator)
            rotatedPoints.drop(1).toList
          }
        }
        // The rotation didn't discover any new interesting point, a shrink is performed
        else {
          val shrunkSimplex = mdsState.getShrink(evaluator, feasReg)
          mdsState.applyMultiPointTransformation(shrunkSimplex, comparator)
          if (mdsState.getSmallestEdge < tolerance) {
            mdsState.reinitializeSimplex(evaluator, feasReg, comparator)
            return mdsState.simplex.drop(1).toList
          }
          else
            mdsState.simplex.drop(1).toList
        }
      }
      case _ => throw new IllegalArgumentException("The MultiDirectional Direct Searc algorithm can only be used with a state for Nelder-Mead")
    }
  }
  
  // TODO: Change this piece of code
  def getInitialState[E <% Ordered[E]](coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    MultiDirectionalSearchState(coordinates, startIntervals, evaluator, feasReg, comparator)
  }
  
}