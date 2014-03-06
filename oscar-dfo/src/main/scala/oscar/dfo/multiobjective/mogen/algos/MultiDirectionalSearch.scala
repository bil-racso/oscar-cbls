package oscar.dfo.multiobjective.mogen.algos

import oscar.dfo.utils.MOOPoint
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.multiobjective.mogen.algos.states.ComparativeAlgorithmState
import oscar.algo.paretofront.ParetoFront
import oscar.algo.paretofront.ParetoElement
import oscar.dfo.utils.FeasibleRegion
import oscar.dfo.multiobjective.mogen.algos.states.MultiDirectionalSearchState

object MultiDirectionalSearch extends ComparativeAlgorithm {
  val algoName = "MultiDirectionalSearch"
  var tolerance = math.pow(10.0, -3.0)
  
  def singleIteration[T <: ParetoElement[Double]](state: ComparativeAlgorithmState, currentArchive: ParetoFront[Double, T], feasReg: FeasibleRegion, evaluator: MOEvaluator): List[MOOPoint] = {
    state match {
      case mdsState: MultiDirectionalSearchState => {
        val rotatedPoints = mdsState.getRotation(evaluator, feasReg)
        val goodRotatedPoints = rotatedPoints.drop(1).filter(candidatePoint => currentArchive.cmpWithArchive(candidatePoint, mdsState.bestPoint))
        // The rotation discovered some new interesting points
        if (goodRotatedPoints.length > 0) {
          // If rotated points are even better than former best point, an expansion is performed
          if (goodRotatedPoints.filter(candidatePoint => !currentArchive.cmpWithArchive(mdsState.bestPoint, candidatePoint)).length > 0) {
            val expandedPoints = mdsState.getExpansion(evaluator, feasReg)
            val rotationScores = rotatedPoints.drop(1).map(point => (point, currentArchive.score(point))).sortWith((pair1, pair2) => pair1._2 >= pair2._2)
            val expansionScores = expandedPoints.drop(1).map(point => (point, currentArchive.score(point))).sortWith((pair1, pair2) => pair1._2 >= pair2._2)
            val bestRotatedPoint = rotationScores(0)
            val bestExpansionPoint = expansionScores(0)
            // Expanded points are more interesting than rotated points
            if ((bestExpansionPoint._1.dominance(bestRotatedPoint._1) + bestExpansionPoint._2 - bestRotatedPoint._2) > 0) {
              mdsState.applyMultiPointTransformation(expandedPoints)
              expandedPoints.drop(1).toList
            }
            // Rotated points are more interesting than expanded points
            else {
              mdsState.applyMultiPointTransformation(rotatedPoints)
              rotatedPoints.drop(1).toList
            }
          }
          else {
            mdsState.applyMultiPointTransformation(rotatedPoints)
            rotatedPoints.drop(1).toList
          }
        }
        // The rotation didn't discover any new interesting point, a shrink is performed
        else {
          val shrunkSimplex = mdsState.getShrink(evaluator, feasReg)
          mdsState.applyMultiPointTransformation(shrunkSimplex)
          mdsState.simplex.drop(1).toList
        }
      }
      case _ => throw new IllegalArgumentException("The MultiDirectional Direct Searc algorithm can only be used with a state for Nelder-Mead")
    }
  }
  
  def getInitialState(coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator, feasReg: FeasibleRegion): ComparativeAlgorithmState = {
    MultiDirectionalSearchState(coordinates, startIntervals, evaluator, feasReg)
  }
}