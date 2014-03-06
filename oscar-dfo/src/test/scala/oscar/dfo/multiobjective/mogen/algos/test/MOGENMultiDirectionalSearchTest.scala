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
import oscar.dfo.multiobjective.mogen.algos.states.MultiDirectionalSearchState
import oscar.dfo.multiobjective.mogen.algos.MultiDirectionalSearch

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class MOGENMultiDirectionalSearchTest extends FunSuite with ShouldMatchers {  
  
  def unitFunction(coordinates: Array[Double]): Array[Double] = {
    coordinates.clone
  }
  
  def inUnitHV(coordinates: Array[Double]): Boolean = {
    for (coord <- coordinates) {
      if (coord < 0 || coord > 1) return false
    }
    true
  }
  
  test("MultiDirectionalSearchState getRotation, getExpansion, getShrink should return the correct new set of points") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val mdsState = MultiDirectionalSearchState(initSimplex, unitInterval)
    mdsState.gammaR = 1.0
    mdsState.gammaE = 2.0
    mdsState.gammaS = 0.5
    val rotation = mdsState.getRotation(evaluator, feasibleRegion)
    rotation(0).coordinates should be (Array(0.0, 0.0))
    rotation(1).coordinates should be (Array(-1.0, 0.0))
    rotation(2).coordinates should be (Array(0.0, -1.0))
    val expansion = mdsState.getExpansion(evaluator, feasibleRegion)
    expansion(0).coordinates should be (Array(0.0, 0.0))
    expansion(1).coordinates should be (Array(-2.0, 0.0))
    expansion(2).coordinates should be (Array(0.0, -2.0))
    val shrink = mdsState.getShrink(evaluator, feasibleRegion)
    shrink(0).coordinates should be (Array(0.0, 0.0))
    shrink(1).coordinates should be (Array(0.5, 0.0))
    shrink(2).coordinates should be (Array(0.0, 0.5))
    mdsState.applyMultiPointTransformation(expansion)
    mdsState.simplex(0).coordinates should be (Array(0.0, 0.0))
    mdsState.simplex(1).coordinates should (be (Array(-2.0, 0.0)) or be (Array(0.0, -2.0)))
    mdsState.simplex(2).coordinates should (be (Array(-2.0, 0.0)) or be (Array(0.0, -2.0)))
  }
  
  test("MultiDirectionalSearch singleIteration should behave correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val initSimplex = Array(evaluator.eval(Array(0.0, 0.0), feasibleRegion),
        evaluator.eval(Array(1.0, 0.0), feasibleRegion),
        evaluator.eval(Array(0.0, 1.0), feasibleRegion))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val mdsState = MultiDirectionalSearchState(initSimplex, unitInterval)
    mdsState.gammaR = 1.0
    mdsState.gammaE = 2.0
    mdsState.gammaS = 0.5
    val triplet = MOGENTriplet(mdsState.getBestPoint, NelderMead, mdsState)
    val archive = LinearListDouble[MOGENTriplet]()
    archive.insert(triplet)
    val res = MultiDirectionalSearch.singleIteration(mdsState, archive, feasibleRegion, evaluator)
    res.length should be (2)
    res(0).coordinates should (be (Array(0.5, 0.0)) or be (Array(0.0, 0.5)))
  }
  
}
