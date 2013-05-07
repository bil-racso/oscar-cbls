package oscar.dfo.mogen.algos

import oscar.dfo.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.mogen.algos.states.NelderMeadState
import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.ParetoFront
import oscar.dfo.mogen.utils.ArrayUtils

object NelderMead extends ComparativeAlgorithm {
  
  var tolerance = math.pow(10.0, -3.0)
  
  def singleIteration[E](state: ComparativeAlgorithmState[E], currentArchive: ParetoFront[E], feasReg: FeasibleRegion, comparator: MOOComparator[E], evaluator: MOEvaluator[E]): List[MOOPoint[E]] = {
    state match {
      case nmState: NelderMeadState[E] => {
        val centroid = nmState.getCentroid
        val reflectedPoint = nmState.getReflection(evaluator, feasReg, centroid)
        // The reflected point is better than the second worst point of the simplex but worse or equivalent to the best point (f^0 <= f^r < f^(n-1) for SO minimisation problems)
        if (comparator.cmpWithArchive(nmState.bestPoint, reflectedPoint, currentArchive) && comparator.cmpWithArchive(reflectedPoint, nmState.simplex(nmState.simplexSize - 2), currentArchive) &&
            !currentArchive.contains(reflectedPoint) && feasReg.isFeasible(reflectedPoint.coordinates)) {
          nmState.applySinglePointTransformation(reflectedPoint, comparator)
          return List(reflectedPoint)
        }
        else {
          // The reflected point was better than the best point of the simplex => Expansion performed
          if (comparator.cmpWithArchive(reflectedPoint, nmState.bestPoint, currentArchive)) {
            val expandedPoint = nmState.getExpansion(evaluator, feasReg, centroid)
            // The expanded point is better than the reflected point
            if (comparator.cmpWithArchive(expandedPoint, reflectedPoint, currentArchive)) {
              nmState.applySinglePointTransformation(expandedPoint, comparator)
              return List(expandedPoint)
            }
            // The reflected point is better than the expanded point
            else {
              nmState.applySinglePointTransformation(reflectedPoint, comparator)
              return List(reflectedPoint)
            }
          }
          // The reflected point was worse than the second worse point of the vertex (f^(n-1) <= f^r for SO minimisation problems)
          else {
            val contractedPoint =
            // The reflected point is better than the worst point of the simplex => Outside contraction
            if (comparator.cmpWithArchive(reflectedPoint, nmState.worstPoint, currentArchive)) nmState.getOutsideContraction(evaluator, feasReg, centroid)
            // The reflected point is worse than the worst point of the simplex => Inside contraction
            else nmState.getInsideContraction(evaluator, feasReg, centroid)
            // The contracted point is better than the reflected point
            if (comparator.cmpWithArchive(contractedPoint, reflectedPoint, currentArchive)) {
              nmState.applySinglePointTransformation(contractedPoint, comparator)
              return List(contractedPoint)
            }
            // The contracted point is worse than the reflected point => shrink
            else {
              nmState.applyShrink(comparator, evaluator, feasReg)
              if (nmState.getSmallestEdge < tolerance) {
                nmState.reinitializeSimplex(evaluator, feasReg, comparator)
              }
              return nmState.simplex.drop(1).toList
            }
          }
        }
      }
      case _ => throw new IllegalArgumentException("The Nelder-Mead algorithm can only be used with a state for Nelder-Mead");
    }
  }
  
  
  def getInitialState[E <% Ordered[E]](coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    NelderMeadState(coordinates, startIntervals, evaluator, feasReg, comparator)
  }
}