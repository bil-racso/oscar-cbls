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
package oscar.dfo.multiobjective.mogen.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.dfo.multiobjective.mogen.MOGEN
import oscar.dfo.multiobjective.mogen.algos.DirectionalDirectSearch
import oscar.dfo.utils.MOEvaluator

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class MOGENTest extends FunSuite with ShouldMatchers {  
  def unitFunction(coordinates: Array[Double]): Array[Double] = {
    coordinates.clone
  }
  
  def unitOppositeFunction(coordinates: Array[Double]): Array[Double] = {
    Array.tabulate(coordinates.length)(i => if (i % 2 == 0) coordinates(i) else -1.0 * coordinates(i))
  }
  
  def inUnitHV(coordinates: Array[Double]): Boolean = {
    for (coord <- coordinates) {
      if (coord < 0 || coord > 1) return false
    }
    true
  }
  
  test("MOGEN initRandomArchive should generate points within init intervals") {
    val nCoordinates = 3
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val mogenSolver = MOGEN(evaluator)

    mogenSolver.initFeasibleReagion(List(inUnitHV))
    mogenSolver.initRandomArchive(10, Array.fill(nCoordinates)((0.0, 1.0)), List((DirectionalDirectSearch, 1.0)))
    mogenSolver.archive.size should (be >= (1) and be <= (10))
    for (element <- mogenSolver.archive.elements) {
      for (coord <- element.coordinates) {
        coord should (be <= (1.0) and be >= (0.0))
      }
    }
  }
  
  test("MOGEN initSinglePointArchive should generate a single point within init intervals") {
    val nCoordinates = 3
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val mogenSolver = MOGEN(evaluator)
    DirectionalDirectSearch.pollingHeuristic = DirectionalDirectSearch.unitPolling
    
    mogenSolver.initFeasibleReagion(List(inUnitHV))
    mogenSolver.initSinglePointArchive(Array.fill(nCoordinates)((0.0, 1.0)), List((DirectionalDirectSearch, 1.0)))
    mogenSolver.archive.size should be (1)
    for (element <- mogenSolver.archive.elements) {
      for (coord <- element.coordinates) {
        coord should (be <= (1.0) and be >= (0.0))
      }
    }
  }
  
  test("MOGEN initLineArchive should generate points within init intervals on a line") {
    val nCoordinates = 3
    val evaluator = MOEvaluator(unitOppositeFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val mogenSolver = MOGEN(evaluator)
    DirectionalDirectSearch.pollingHeuristic = DirectionalDirectSearch.unitPolling
    
    mogenSolver.initFeasibleReagion(List(inUnitHV))
    mogenSolver.initLineArchive(10, Array.fill(nCoordinates)((0.0, 1.0)), List((DirectionalDirectSearch, 1.0)))
    mogenSolver.archive.size should be (10)
    for (element <- mogenSolver.archive.elements) {
      for (coord <- element.coordinates) {
        coord should (be <= (1.0) and be >= (0.0))
      }
    }
  }
}
