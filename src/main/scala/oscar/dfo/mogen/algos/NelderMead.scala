package oscar.dfo.mogen.algos

import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.ParetoFront
import oscar.dfo.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.mogen.algos.states.NelderMeadState

object NelderMead extends ComparativeAlgorithm {
  def singleIteration[E](state: ComparativeAlgorithmState[E], currentArchive: ParetoFront[E], feasReg: FeasibleRegion, comparator: MOOComparator[E], evaluator: MOEvaluator[E]): List[MOOPoint[E]] = {
    state match {
      case nmState: NelderMeadState[E] => {
        val centroid = nmState.getCentroid
        val reflectedPoint = nmState.getReflection(evaluator, feasReg, centroid)
        //println("=" * 30 + " " * 5 + "SIMPLEX" + " " * 5 + "=" * 30)
        //for (point <- nmState.simplex) println(point)
        //println("=" * 30 + " " * 5 + "REFLEXION" + " " * 5 + "=" * 30)
        //println(reflectedPoint )
        //println("=" * 80)
        // The reflected point is better than the second worst point of the simplex but worse or equivalent to the best point (f^0 <= f^r < f^(n-1) for SO minimisation problems)
        if (comparator.cmpWithArchive(nmState.bestPoint, reflectedPoint, currentArchive) && comparator.cmpWithArchive(reflectedPoint, nmState.simplex(nmState.simplexSize - 2), currentArchive) &&
            !currentArchive.contains(reflectedPoint) && feasReg.isFeasible(reflectedPoint.coordinates)) {
          nmState.applySinglePointTransformation(reflectedPoint, comparator)
          println("Relected Point: " + reflectedPoint)
          return List(reflectedPoint)
        }
        else {
          // The reflected point was better than the best point of the simplex => Expansion performed
          if (comparator.cmpWithArchive(reflectedPoint, nmState.bestPoint, currentArchive)) {
            val expandedPoint = nmState.getExpansion(evaluator, feasReg, centroid)
            //println("reflectedPoint: " + reflectedPoint)
            //println("bestPoint: " + nmState.bestPoint)
            //println("expandedPoint: " + expandedPoint)
            // The expanded point is better than the reflected point
            if (comparator.cmpWithArchive(expandedPoint, reflectedPoint, currentArchive)) {
              nmState.applySinglePointTransformation(expandedPoint, comparator)
              println("Expanded Point better than reflected Point")
              return List(expandedPoint)
            }
            // The reflected point is better than the expanded point
            else {
              println("Expanded Point worse than relected Point")
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
              println("Contraction")
              nmState.applySinglePointTransformation(contractedPoint, comparator)
              return List(contractedPoint)
            }
            // The contracted point is worse than the reflected point => shrink
            else {
              println("Shrink")
              nmState.applyShrink(comparator, evaluator, feasReg)
              return nmState.simplex.toList
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