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
package oscar.dfo.multiobjective.mogen.algos.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.dfo.multiobjective.mogen.MOGEN
import oscar.dfo.multiobjective.mogen.algos.DirectionalDirectSearch
import oscar.dfo.multiobjective.mogen.algos.NelderMead
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.multiobjective.mogen.algos.states.NelderMeadState
import oscar.dfo.utils.FeasibleRegion
import oscar.algo.paretofront.LinearListDouble
import oscar.dfo.multiobjective.mogen.MOGENTriplet

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class MOGENNelderMeadTest extends FunSuite with ShouldMatchers {  
  
  def unitFunction(coordinates: Array[Double]): Array[Double] = {
    coordinates.clone
  }
  
  def inUnitHV(coordinates: Array[Double]): Boolean = {
    for (coord <- coordinates) {
      if (coord < 0 || coord > 1) return false
    }
    true
  }
  
  test("NelderMeadState getCentroid should return the correct centroid") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val nmState = NelderMeadState(initSimplex, unitInterval)
    nmState.getCentroid should be (Array(0.5, 0.0))
  }
  
  test("NelderMeadState getReflection, getExpansion, getInsideContraction and getOutsideContraction should return correct new points") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val nmState = NelderMeadState(initSimplex, unitInterval)
    nmState.getReflection(evaluator, feasibleRegion).coordinates should be (Array(1.0, -1.0))
    nmState.getExpansion(evaluator, feasibleRegion).coordinates should be (Array(1.5, -2.0))
    nmState.getInsideContraction(evaluator, feasibleRegion).coordinates should be (Array(0.25, 0.5))
    nmState.getOutsideContraction(evaluator, feasibleRegion).coordinates should be (Array(0.75, -0.5))
  }
  
  test("NelderMeadState apllyShrink should modify the simplex correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val nmState = NelderMeadState(initSimplex, unitInterval)
    nmState.applyShrink(evaluator, feasibleRegion)
    nmState.simplex(0).coordinates should be (Array(0.0, 0.0))
    nmState.simplex(1).coordinates should (be (Array(0.5, 0.0)) or be (Array(0.0, 0.5)))
    nmState.simplex(2).coordinates should (be (Array(0.5, 0.0)) or be (Array(0.0, 0.5)))
  }
  
  test("NelderMeadState applySinglePointTransformation should modify the simplex correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val nmState = NelderMeadState(initSimplex, unitInterval)
    val contraction = nmState.getInsideContraction(evaluator, feasibleRegion)
    nmState.applySinglePointTransformation(contraction)
    nmState.simplex(0).coordinates should be (Array(0.0, 0.0))
    nmState.simplex(1).coordinates should (be (Array(0.25, 0.5)) or be (Array(1.0, 0.0)))
    nmState.simplex(2).coordinates should (be (Array(0.25, 0.5)) or be (Array(1.0, 0.0)))
  }
  
  test("NelderMeadState orderSimplex should order the simplex correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val nmState = NelderMeadState(initSimplex, unitInterval)
    nmState.orderSimplex
    nmState.simplex(0).coordinates should be (Array(0.0, 0.0))
    nmState.simplex(1).coordinates should (be (Array(0.0, 1.0)) or be (Array(1.0, 0.0)))
    nmState.simplex(2).coordinates should (be (Array(0.0, 1.0)) or be (Array(1.0, 0.0)))
    val newBestPoint = nmState.simplex(2)
    val newWorstPoint = nmState.simplex(1)
    nmState.bestPoint = newBestPoint
    nmState.orderSimplex
    nmState.simplex(0).coordinates should be (newBestPoint.coordinates)
    nmState.simplex(1).coordinates should be (Array(0.0, 0.0))
    nmState.simplex(2).coordinates should be (newWorstPoint.coordinates)
  }
  
  test("Simplex trait utilary functions should work correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val nmState = NelderMeadState(initSimplex, unitInterval)
    nmState.simplexSize should be (nCoordinates + 1)
    nmState.nCoordinates should be (nCoordinates)
    val array1 = Array(1.0, 3.2, -8.0)
    val array2 = Array(-4.2, 2.1, 3.5)
    val sum = nmState.arraySum(array1, array2)
    sum(0) should be (-3.2 plusOrMinus 0.00001)
    sum(1) should be (5.3 plusOrMinus 0.00001)
    sum(2) should be (-4.5 plusOrMinus 0.00001)
    val diff = nmState.arrayDiff(array1, array2)
    diff(0) should be (5.2 plusOrMinus 0.00001)
    diff(1) should be (1.1 plusOrMinus 0.00001)
    diff(2) should be (-11.5 plusOrMinus 0.00001)
    val prod = nmState.arrayProd(array1, 42.0)
    prod(0) should be (42.0 plusOrMinus 0.00001)
    prod(1) should be (134.4 plusOrMinus 0.00001)
    prod(2) should be (-336.0 plusOrMinus 0.00001)
  }
  
  test("NelderMead singleIteration should behave correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val nmState = NelderMeadState(initSimplex, unitInterval)
    val triplet = MOGENTriplet(nmState.getBestPoint, NelderMead, nmState)
    val archive = LinearListDouble[MOGENTriplet]()
    archive.insert(triplet)
    val res = NelderMead.singleIteration(nmState, archive, feasibleRegion, evaluator)
    res(0).coordinates should be(Array(0.25, 0.5))
  }
}
