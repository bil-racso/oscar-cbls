package oscar.dfo.mogen.algos.states

import oscar.dfo.mogen.utils.Simplex
import oscar.util.mo.MOOPoint
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOEvaluator
import oscar.util.mo.FeasibleRegion
import oscar.dfo.mogen.utils.ArrayUtils
import oscar.util.mo.RandomGenerator

class MultiDirectionalSearchState[E <% Ordered[E]](simplexInit: Array[MOOPoint[E]], val intervals: Array[(Double, Double)]) extends ComparativeAlgorithmState[E] with Simplex[E] {
  val simplex = Array.tabulate(simplexInit.length)(i => simplexInit(i))
  var bestPoint = simplex(0)
  var gammaS = 0.5
  var gammaE = 2.0
  var gammaR = 1.0
  
  def getNewState(newBestPoint: MOOPoint[E], comparator: MOOComparator[E]): MultiDirectionalSearchState[E] = {
    val newState = MultiDirectionalSearchState(simplex, intervals)
    newState.gammaS = this.gammaS
    newState.gammaE = this.gammaE
    newState.gammaR = this.gammaR
    newState.bestPoint = newBestPoint
    newState.orderSimplex(comparator)
    newState
  }
  
  def getRotation(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion): Array[MOOPoint[E]] = getMultiPointTransformation(gammaR, evaluator, feasibleReg)
  
  def getExpansion(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion): Array[MOOPoint[E]] = getMultiPointTransformation(gammaE, evaluator, feasibleReg)
  
  def getShrink(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion): Array[MOOPoint[E]] = getMultiPointTransformation(-gammaS, evaluator, feasibleReg)
  
  def getMultiPointTransformation(factor: Double, evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion): Array[MOOPoint[E]] = {
    Array.tabulate(simplex.length){i =>
      if (i == 0) bestPoint
      else evaluator.eval(ArrayUtils.arrayDiff(bestPoint.coordinates, ArrayUtils.arrayProd(ArrayUtils.arrayDiff(simplex(i).coordinates, bestPoint.coordinates), factor)), feasibleReg)
    }
  }
  
  def applyMultiPointTransformation(newSimplex: Array[MOOPoint[E]], comparator: MOOComparator[E]) {
    for (i <- 0 until simplex.length)
      simplex(i) = newSimplex(i)
    orderSimplex(comparator)
  }
  
  def reinitializeSimplex(evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): Unit = reinitializeSimplex(intervals, evaluator, feasReg, comparator)
}

object MultiDirectionalSearchState {
  def apply[E <% Ordered[E]](simplexInit: Array[MOOPoint[E]], intervals: Array[(Double, Double)]): MultiDirectionalSearchState[E] = new MultiDirectionalSearchState(simplexInit, intervals)
  
  def apply[E <% Ordered[E]](coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): MultiDirectionalSearchState[E] = {
    val simplex = Array.tabulate(coordinates.length + 1){ index =>
      if (index == 0) coordinates
      else {
        val randPerturbation = startIntervals.map(e => (0.5 - RandomGenerator.nextDouble) * math.abs(e._2 - e._1))
        Array.tabulate(coordinates.length)(i => coordinates(i) + randPerturbation(i))
      }
    }
    MultiDirectionalSearchState(simplex.map(coord => evaluator.eval(coord, feasReg)), startIntervals)
  }
}