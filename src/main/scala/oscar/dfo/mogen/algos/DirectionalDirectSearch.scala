package oscar.dfo.mogen.algos

import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.ParetoFront
import oscar.dfo.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.mogen.algos.states.DirectionalDirectSearchState

object DirectionalDirectSearch extends ComparativeAlgorithm {
  var tolerance = math.pow(10.0, -3.0)
  
  def singleIteration[E](state: ComparativeAlgorithmState[E], currentArchive: ParetoFront[E], feasReg: FeasibleRegion, comparator: MOOComparator[E], evaluator: MOEvaluator[E]): List[MOOPoint[E]] = {
    state match {
      case ddsState: DirectionalDirectSearchState[E] => {
        for (i <- 0 until ddsState.basisSize) {
          val newPoint = ddsState.getNewPoint(i, evaluator, feasReg)
          println("NewPoint: " + newPoint)
          println("StepSizes Before: " + ddsState.stepSizes.mkString("(", ", ", ")"))
          if (comparator.cmpWithArchive(newPoint, ddsState.bestPoint, currentArchive)) {
            ddsState.updateBasis
            ddsState.increaseStepSizes
            return List(newPoint)
          }
        }
        ddsState.updateBasis
        ddsState.decreaseStepSizes
        println("StepSizes After: " + ddsState.stepSizes.mkString("(", ", ", ")"))
        List[MOOPoint[E]]()
      }
      case _ => throw new IllegalArgumentException("The Directional Direct-Search algorithm can only be used with a state for Directional Direct-Search");
    }
  }
  
  
  def getInitialState[E <% Ordered[E]](coordinates: Array[Double], stepSizeIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): ComparativeAlgorithmState[E] = {
    DirectionalDirectSearchState(evaluator.eval(coordinates, feasReg), stepSizeIntervals)
  }
}