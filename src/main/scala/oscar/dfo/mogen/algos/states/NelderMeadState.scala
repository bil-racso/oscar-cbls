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
package oscar.dfo.mogen.algos.states

import scala.Array.canBuildFrom
import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MOOComparator
import oscar.util.mo.MOOPoint
import oscar.util.mo.RandomGenerator
import oscar.dfo.mogen.utils.ArrayUtils
import oscar.dfo.mogen.utils.Simplex

class NelderMeadState[E <% Ordered[E]](simplexInit: Array[MOOPoint[E]], val intervals: Array[(Double, Double)]) extends ComparativeAlgorithmState[E] with Simplex[E] {
  val simplex = Array.tabulate(simplexInit.length)(i => simplexInit(i))
  var bestPoint = simplex(0)

  var deltaR = 1
  var deltaE = 2
  var deltaOC = 0.5
  var deltaIC = -0.5
  var gammaS = 0.5
  
  def getNewState(newBestPoint: MOOPoint[E], comparator: MOOComparator[E]): NelderMeadState[E] = {
    val newState = NelderMeadState(simplex, intervals)
    newState.deltaR = this.deltaR
    newState.deltaE = this.deltaE
    newState.deltaOC = this.deltaOC
    newState.deltaIC = this.deltaIC
    newState.gammaS = this.gammaS
    newState.bestPoint = newBestPoint
    newState.orderSimplex(comparator)
    newState
  }
  
  def getReflection(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaR, evaluator, feasibleReg)
  
  def getExpansion(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaE, evaluator, feasibleReg)
  
  def getInsideContraction(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaIC, evaluator, feasibleReg)
  
  def getOutsideContraction(evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion, centroid: Array[Double] = getCentroid): MOOPoint[E] = getSinglePointTransformation(centroid, deltaOC, evaluator, feasibleReg)
  
  def getSinglePointTransformation(centroid: Array[Double], factor: Double, evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion): MOOPoint[E] = {
    val newCoordinates = ArrayUtils.arraySum(centroid, ArrayUtils.arrayProd(ArrayUtils.arrayDiff(centroid, worstPoint.coordinates), factor))
    evaluator.eval(newCoordinates, feasibleReg)
  }
  
  def applySinglePointTransformation(newPoint: MOOPoint[E], comparator: MOOComparator[E]) = {
    simplex(simplexSize - 1) = newPoint
    orderSimplex(comparator)
  }
  
  def getCentroid: Array[Double] = {
    val allButWorstCoordinates = simplex.map(mooP => mooP.coordinates).take(simplexSize - 1)
    ArrayUtils.arrayProd(allButWorstCoordinates.drop(1).foldLeft(allButWorstCoordinates(0))((acc, newCoords) => ArrayUtils.arraySum(acc, newCoords)), 1.0 / (simplexSize - 1))
  }
  
  def applyShrink(comparator: MOOComparator[E], evaluator: MOEvaluator[E], feasibleReg: FeasibleRegion) = {
    val simplexCoordinates = simplex.map(mooP => mooP.coordinates)
    for (i <- 1 until simplexSize) {
      simplex(i) = evaluator.eval(ArrayUtils.arraySum(simplexCoordinates(0), ArrayUtils.arrayProd(ArrayUtils.arrayDiff(simplexCoordinates(i), simplexCoordinates(0)), gammaS)), feasibleReg)
    }
    printSimplex
    orderSimplex(comparator)
  }
  
  def reinitializeSimplex(evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): Unit = reinitializeSimplex(intervals, evaluator, feasReg, comparator)
}

object NelderMeadState {
  def apply[E <% Ordered[E]](simplex: Array[MOOPoint[E]], intervals: Array[(Double, Double)]) = new NelderMeadState(simplex, intervals)
  
  def apply[E <% Ordered[E]](coordinates: Array[Double], startIntervals: Array[(Double, Double)], evaluator: MOEvaluator[E], feasReg: FeasibleRegion, comparator: MOOComparator[E]): NelderMeadState[E] = {
    val simplex = Array.tabulate(coordinates.length + 1){ index =>
      if (index == 0) coordinates
      else {
        val randPerturbation = startIntervals.map(e => ((2 * RandomGenerator.nextDouble) - 1) * math.abs(e._2 - e._1))
        Array.tabulate(coordinates.length)(i => coordinates(i) + randPerturbation(i))
      }
    }
    NelderMeadState(simplex.map(coord => evaluator.eval(coord, feasReg)), startIntervals)
  }
}
