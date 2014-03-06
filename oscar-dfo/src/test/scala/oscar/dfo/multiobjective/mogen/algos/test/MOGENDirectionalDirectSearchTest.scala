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
import oscar.dfo.multiobjective.mogen.algos.states.DirectionalDirectSearchState

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class MOGENDirectionalDirectSearchTest extends FunSuite with ShouldMatchers {  
  
  def unitFunction(coordinates: Array[Double]): Array[Double] = {
    coordinates.clone
  }
  
  def inUnitHV(coordinates: Array[Double]): Boolean = {
    for (coord <- coordinates) {
      if (coord < 0 || coord > 1) return false
    }
    true
  }
  
  def vectorLength(tab: Array[Double]): Double = {
    math.sqrt(tab.foldLeft(0.0)((acc, coord) => acc + coord * coord))
  }
  
  test("DirectionalDirectSearchState increase/decrease step size should work correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val coordinates = Array.fill(nCoordinates)(0.5)
    val alpha = 0.25
    val ddsState = DirectionalDirectSearchState(evaluator.eval(coordinates, feasibleRegion), alpha)
    DirectionalDirectSearchState.increaseFactor = 2.0
    DirectionalDirectSearchState.decreaseFactor = 0.5
    ddsState.increaseStepSize
    ddsState.alpha should be (0.5 plusOrMinus 0.00001)
    ddsState.reinitialize
    ddsState.alpha should be (0.25 plusOrMinus 0.00001)
    ddsState.decreaseStepSize
    ddsState.alpha should be (0.125 plusOrMinus 0.00001)
  }
  
  test("DirectionalDirectSearchState getPollCoordinate should work correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val coordinates = Array.fill(nCoordinates)(0.5)
    val alpha = 0.25
    val ddsState = DirectionalDirectSearchState(evaluator.eval(coordinates, feasibleRegion), alpha)
    val posDirections = Array.tabulate(nCoordinates)(i => Array.tabulate(nCoordinates)(j => if (i==j) 1.0 else 0.0))
    val negDirections = Array.tabulate(nCoordinates)(i => Array.tabulate(nCoordinates)(j => if (i==j) -1.0 else 0.0))
    val directions = posDirections ++ negDirections
    val pollCoordinates = ddsState.getPollCoordinates(directions)
    pollCoordinates(0)(0) should be (0.75 plusOrMinus 0.00001)
    pollCoordinates(0)(1) should be (0.5 plusOrMinus 0.00001)
    pollCoordinates(1)(0) should be (0.5 plusOrMinus 0.00001)
    pollCoordinates(1)(1) should be (0.75 plusOrMinus 0.00001)
    pollCoordinates(2)(0) should be (0.25 plusOrMinus 0.00001)
    pollCoordinates(2)(1) should be (0.5 plusOrMinus 0.00001)
    pollCoordinates(3)(0) should be (0.5 plusOrMinus 0.00001)
    pollCoordinates(3)(1) should be (0.25 plusOrMinus 0.00001)
  }
  
  test("DirectionalDirectSearch unitPolling should work correctly") {
    val nCoordinates = 3
    val pollDirections = DirectionalDirectSearch.unitPolling(nCoordinates)
    pollDirections(0) should be (Array(1.0, 0.0, 0.0))
    pollDirections(1) should be (Array(0.0, 1.0, 0.0))
    pollDirections(2) should be (Array(0.0, 0.0, 1.0))
    pollDirections(3) should be (Array(-1.0, 0.0, 0.0))
    pollDirections(4) should be (Array(0.0, -1.0, 0.0))
    pollDirections(5) should be (Array(0.0, 0.0, -1.0))
  }
  
  test("DirectionalDirectSearch randomPolling should work correctly") {
    val nCoordinates = 3
    val pollDirections = DirectionalDirectSearch.randomPolling(nCoordinates)
    for (i <- 0 until (nCoordinates * 2)) {
      vectorLength(pollDirections(i)) should be (1.0 plusOrMinus 0.00001)
    }
  }
  
  test("DirectionalDirectSearch singleIteration should behave correctly") {
    val nCoordinates = 2
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val feasibleRegion = FeasibleRegion(List(inUnitHV))
    val unitInterval = Array.fill(nCoordinates)((0.0, 1.0))
    val coordinates = Array.fill(nCoordinates)(0.5)
    val alpha = 0.25
    val ddsState = DirectionalDirectSearchState(evaluator.eval(coordinates, feasibleRegion), alpha)
    DirectionalDirectSearch.pollingHeuristic = DirectionalDirectSearch.unitPolling
    val triplet = MOGENTriplet(ddsState.getBestPoint, NelderMead, ddsState)
    val archive = LinearListDouble[MOGENTriplet]()
    archive.insert(triplet)
    val res = DirectionalDirectSearch.singleIteration(ddsState, archive, feasibleRegion, evaluator)
    res.length should be (2)
    res(0).coordinates should (be (Array(0.25, 0.5)) or be (Array(0.5, 0.25)))
    res(1).coordinates should (be (Array(0.25, 0.5)) or be (Array(0.5, 0.25)))
  }
}