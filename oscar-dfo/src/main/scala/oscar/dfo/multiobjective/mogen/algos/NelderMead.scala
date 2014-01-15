/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.dfo.multiobjective.mogen.algos

import oscar.dfo.utils._
import oscar.dfo.multiobjective.mogen.algos.states.ComparativeAlgorithmState
import oscar.dfo.multiobjective.mogen.algos.states.NelderMeadState
import oscar.dfo.multiobjective.mogen.MOGENTriplet
import oscar.algo.paretofront.ParetoFront
import oscar.algo.paretofront.ParetoElement

object NelderMead extends ComparativeAlgorithm {
  def singleIteration[T <: ParetoElement[Double]](state: ComparativeAlgorithmState, currentArchive: ParetoFront[Double, T], feasReg: FeasibleRegion, evaluator: MOEvaluator): List[MOOPoint] = {
    state match {
      case nmState: NelderMeadState => {
        val centroid = nmState.getCentroid
        val reflectedPoint = nmState.getReflection(evaluator, feasReg, centroid)
        // The reflected point is better than the second worst point of the simplex but worse or equivalent to the best point (f^0 <= f^r < f^(n-1) for SO minimisation problems)
        if (currentArchive.cmpWithArchive(nmState.bestPoint, reflectedPoint) && currentArchive.cmpWithArchive(reflectedPoint, nmState.simplex(nmState.simplexSize - 2)) &&
            !currentArchive.contains(reflectedPoint) && feasReg.isFeasible(reflectedPoint.coordinates)) {
          nmState.applySinglePointTransformation(reflectedPoint)
          return List(reflectedPoint)
        }
        else {
          // The reflected point was better than the best point of the simplex => Expansion performed
          if (currentArchive.cmpWithArchive(reflectedPoint, nmState.bestPoint)) {
            val expandedPoint = nmState.getExpansion(evaluator, feasReg, centroid)
            if (currentArchive.cmpWithArchive(expandedPoint, reflectedPoint)) {
              nmState.applySinglePointTransformation(expandedPoint)
              return List(expandedPoint)
            }
            // The reflected point is better than the expanded point
            else {
              nmState.applySinglePointTransformation(reflectedPoint)
              return List(reflectedPoint)
            }
          }
          // The reflected point was worse than the second worse point of the vertex (f^(n-1) <= f^r for SO minimisation problems)
          else {
            val contractedPoint =
            // The reflected point is better than the worst point of the simplex => Outside contraction
            if (currentArchive.cmpWithArchive(reflectedPoint, nmState.worstPoint)) nmState.getOutsideContraction(evaluator, feasReg, centroid)
            // The reflected point is worse than the worst point of the simplex => Inside contraction
            else nmState.getInsideContraction(evaluator, feasReg, centroid)
            // The contracted point is better than the reflected point
            if (currentArchive.cmpWithArchive(contractedPoint, reflectedPoint)) {
              nmState.applySinglePointTransformation(contractedPoint)
              return List(contractedPoint)
            }
            // The contracted point is worse than the reflected point => shrink
            else {
              nmState.applyShrink(evaluator, feasReg)
              return nmState.simplex.toList
            }
          }
        }
      }
      case _ => throw new IllegalArgumentException("The Nelder-Mead algorithm can only be used with a state for Nelder-Mead");
    }
  }
  
  
  def getInitialState(coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator, feasReg: FeasibleRegion): ComparativeAlgorithmState = {
    NelderMeadState(coordinates, startIntervals, evaluator, feasReg)
  }
}
