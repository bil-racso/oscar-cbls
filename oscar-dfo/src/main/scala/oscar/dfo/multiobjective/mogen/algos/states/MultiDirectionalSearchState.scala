package oscar.dfo.multiobjective.mogen.algos.states

import oscar.util.RandomGenerator
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.FeasibleRegion

class MultiDirectionalSearchState (simplexInit: Array[MOOPoint], val intervals: Array[(Double, Double)]) extends ComparativeAlgorithmState with Simplex[Double] {
  val simplex = Array.tabulate(simplexInit.length)(i => simplexInit(i))
  var bestPoint = simplex(0)
  var gammaS = 0.5
  var gammaE = 2.0
  var gammaR = 1.0
  
  def getNewState(newBestPoint: MOOPoint): MultiDirectionalSearchState = {
    val newState = MultiDirectionalSearchState(simplex, intervals)
    newState.gammaS = this.gammaS
    newState.gammaE = this.gammaE
    newState.gammaR = this.gammaR
    newState.bestPoint = newBestPoint
    newState.orderSimplex()
    newState
  }
  
  def getRotation(evaluator: MOEvaluator, feasibleReg: FeasibleRegion): Array[MOOPoint] = getMultiPointTransformation(gammaR, evaluator, feasibleReg)
  
  def getExpansion(evaluator: MOEvaluator, feasibleReg: FeasibleRegion): Array[MOOPoint] = getMultiPointTransformation(gammaE, evaluator, feasibleReg)
  
  def getShrink(evaluator: MOEvaluator, feasibleReg: FeasibleRegion): Array[MOOPoint] = getMultiPointTransformation(-gammaS, evaluator, feasibleReg)
}

object MultiDirectionalSearchState {
  def apply(simplexInit: Array[MOOPoint], intervals: Array[(Double, Double)]): MultiDirectionalSearchState = new MultiDirectionalSearchState(simplexInit, intervals)
  
  def apply(coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator, feasReg: FeasibleRegion): MultiDirectionalSearchState = {
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