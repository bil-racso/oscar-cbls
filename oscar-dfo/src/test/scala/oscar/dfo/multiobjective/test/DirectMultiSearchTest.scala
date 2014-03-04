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
package oscar.dfo.multiobjective.test

import scala.util.Random

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.dfo.multiobjective.algos.DirectMultiSearch
import oscar.dfo.multiobjective.algos.DirectMultiSearchElement
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.Utils

class DirectMultiSearchTest extends FunSuite with ShouldMatchers {
  
  def vectorLength(tab: Array[Double]): Double = {
    math.sqrt(tab.foldLeft(0.0)((acc, coord) => acc + coord * coord))
  }
  
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
  
  test("Normalized Arrays should always have a unit length") {
    vectorLength(Utils.normalizeArray(Array(1.0, 0.0))) should be (1.0 plusOrMinus 0.00001)
    vectorLength(Utils.normalizeArray(Array(1.0, 1.0))) should be (1.0 plusOrMinus 0.00001)
    vectorLength(Utils.normalizeArray(Array.tabulate(42)(i => Random.nextDouble * 42))) should be (1.0 plusOrMinus 0.00001)
  }
  
  test("Normalized Arrays should only have effect on vector which have not a unit length") {
    Utils.normalizeArray(Array(1.0, 0.0)) should be (Array(1.0, 0.0))
    Utils.normalizeArray(Array(1.0, 1.0)) should not be (Array(1.0, 1.0))
    Utils.normalizeArray(Array(math.sqrt(2.0) / 2.0, math.sqrt(2.0)/ 2.0)) should be (Array(math.sqrt(2.0) / 2.0, math.sqrt(2.0)/ 2.0))
    val randArray = Array.tabulate(42)(i => Random.nextDouble * 42)
    Utils.normalizeArray(randArray) should not be (randArray)
  }
  
  test("Random Normalized Vector should contain only normalized vectors") {
    vectorLength(Utils.randomNormalizedVector(1)) should be (1.0 plusOrMinus 0.00001)
    vectorLength(Utils.randomNormalizedVector(3)) should be (1.0 plusOrMinus 0.00001)
    vectorLength(Utils.randomNormalizedVector(42)) should be (1.0 plusOrMinus 0.00001)
  }
  
  test("DirectMultiSearch unitPolling should return unit directions") {
    val nCoordinates = 3
    val basis = DirectMultiSearch.unitPolling(nCoordinates)
    basis.length should be (nCoordinates * 2)
    basis(0) should be (Array(1.0, 0.0, 0.0))
    basis(1) should be (Array(0.0, 1.0, 0.0))
    basis(2) should be (Array(0.0, 0.0, 1.0))
    basis(3) should be (Array(-1.0, 0.0, 0.0))
    basis(4) should be (Array(0.0, -1.0, 0.0))
    basis(5) should be (Array(0.0, 0.0, -1.0))
  }
  
  test("DirectMultiSearch randomPolling should return unit directions") {
    val nCoordinates = 3
    val basis = DirectMultiSearch.randomPolling(nCoordinates)
    basis.length should be (nCoordinates * 2)
    vectorLength(basis(0)) should be (1.0 plusOrMinus 0.00001)
    vectorLength(basis(1)) should be (1.0 plusOrMinus 0.00001)
    vectorLength(basis(2)) should be (1.0 plusOrMinus 0.00001)
    vectorLength(basis(3)) should be (1.0 plusOrMinus 0.00001)
    vectorLength(basis(4)) should be (1.0 plusOrMinus 0.00001)
    vectorLength(basis(5)) should be (1.0 plusOrMinus 0.00001)
    basis(3) should be(basis(0).map(coord => -1.0 * coord))
    basis(4) should be(basis(1).map(coord => -1.0 * coord))
    basis(5) should be(basis(2).map(coord => -1.0 * coord))
  }
  
  test("DirectMultiSearchElement should return correct poll coordinates") {
    val nCoordinates = 3
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val DMSSolver = DirectMultiSearch(evaluator)

    DMSSolver.initFeasibleReagion(List(inUnitHV))
    
    val coordinates = Array.fill(nCoordinates)(0.5)
    val alpha = 1.0
    val dmsElement = DirectMultiSearchElement(evaluator.eval(coordinates, DMSSolver.feasibleRegion), alpha)
    val basis = DirectMultiSearch.unitPolling(nCoordinates)
    val pollCoordinates = dmsElement.getPollCoordinates(basis)
    pollCoordinates.length should be (nCoordinates * 2)
    pollCoordinates(0) should be(Array(1.5, 0.5, 0.5))
    pollCoordinates(1) should be(Array(0.5, 1.5, 0.5))
    pollCoordinates(2) should be(Array(0.5, 0.5, 1.5))
    pollCoordinates(3) should be(Array(-0.5, 0.5, 0.5))
    pollCoordinates(4) should be(Array(0.5, -0.5, 0.5))
    pollCoordinates(5) should be(Array(0.5, 0.5, -0.5))
  }
  
  test("DirectMultiSearch initRandomArchive should generate points within init intervals") {
    val nCoordinates = 3
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val DMSSolver = DirectMultiSearch(evaluator)

    DMSSolver.initFeasibleReagion(List(inUnitHV))
    DMSSolver.initRandomArchive(10, Array.fill(nCoordinates)((0.0, 1.0)), 1.0)
    DMSSolver.archive.size should (be >= (1) and be <= (10))
    for (element <- DMSSolver.archive.elements) {
      for (coord <- element.coordinates) {
        coord should (be <= (1.0) and be >= (0.0))
      }
    }
  }
  
  test("DirectMultiSearch initSinglePointArchive should generate a single point within init intervals") {
    val nCoordinates = 3
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val DMSSolver = DirectMultiSearch(evaluator)

    DMSSolver.initFeasibleReagion(List(inUnitHV))
    DMSSolver.initSinglePointArchive(Array.fill(nCoordinates)((0.0, 1.0)), 1.0)
    DMSSolver.archive.size should be (1)
    for (element <- DMSSolver.archive.elements) {
      for (coord <- element.coordinates) {
        coord should (be <= (1.0) and be >= (0.0))
      }
    }
  }
  
  test("DirectMultiSearch initLineArchive should generate points within init intervals on a line") {
    val nCoordinates = 3
    val evaluator = MOEvaluator(unitOppositeFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val DMSSolver = DirectMultiSearch(evaluator)

    DMSSolver.initFeasibleReagion(List(inUnitHV))
    DMSSolver.initLineArchive(10, Array.fill(nCoordinates)((0.0, 1.0)), 1.0)
    DMSSolver.archive.size should be (10)
    for (element <- DMSSolver.archive.elements) {
      for (coord <- element.coordinates) {
        coord should (be <= (1.0) and be >= (0.0))
      }
    }
  }
  
  test("DirectMultiSearch should increase the step parameter correctly") {
    val nCoordinates = 2
    val alpha = 0.1
    val increaseFactor = 2.0
    val decreaseFactor = 0.5
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val DMSSolver = DirectMultiSearch(evaluator)

    DMSSolver.increaseFactor = increaseFactor
    DMSSolver.decreaseFactor = decreaseFactor
    DMSSolver.searchStepProba = 0.0
    DMSSolver.initFeasibleReagion(List(inUnitHV))
    DMSSolver.initSinglePointArchive(Array.fill(nCoordinates)((0.0, 1.0)), alpha)
    DMSSolver.archive.size should be (1)
    DMSSolver.performIteration(0)
    DMSSolver.archive.size should be > (1)
    for (element <- DMSSolver.archive.elements) {
      element.alpha should be (alpha * increaseFactor)
    }
  }
  
  test("DirectMultiSearch should derease the step parameter correctly") {
    val nCoordinates = 2
    val alpha = 5.0
    val increaseFactor = 2.0
    val decreaseFactor = 0.5
    val evaluator = MOEvaluator(unitFunction, Array.fill(nCoordinates)(Double.MaxValue))
    val DMSSolver = DirectMultiSearch(evaluator)
    
    DMSSolver.increaseFactor = increaseFactor
    DMSSolver.decreaseFactor = decreaseFactor
    DMSSolver.searchStepProba = 0.0
    DMSSolver.initFeasibleReagion(List(inUnitHV))
    DMSSolver.initSinglePointArchive(Array.fill(nCoordinates)((0.0, 1.0)), alpha)
    DMSSolver.archive.size should be (1)
    DMSSolver.performIteration(0)
    DMSSolver.archive.size should be (1)
    for (element <- DMSSolver.archive.elements) {
      element.alpha should be (alpha * decreaseFactor)
    }
  }
}
