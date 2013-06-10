/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.dfo.mogen.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.dfo.mogen.algos.states.MultiDirectionalSearchState
import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MOOPoint
import oscar.util.mo.MinMOOComparator

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class TestMultiDirectionalSearch extends FunSuite with ShouldMatchers {
  val originPoint = MOOPoint(Array(0.0, 0.0), Array(0.0, 0.0))
  val point1 = MOOPoint(Array(0.0, 1.0), Array(0.0, 1.0))
  val point2 = MOOPoint(Array(1.0, 1.0), Array(1.0, 1.0))
  val unit2DIntervals = Array((0.0, 1.0), (0.0, 1.0))
  val evaluator = MOEvaluator(dummySquareEval, Array(Double.MaxValue, Double.MaxValue))
  val feasReg = FeasibleRegion(List(inUnitInterval))
  val comparator = MinMOOComparator[Double]()
  
  test("Test MultiDirectionalSearch dummy 2D - Basic initialization of a MultiDirectionalSearchState") {
    val mdsState1 = MultiDirectionalSearchState(Array(originPoint, point1, point2), unit2DIntervals)
    mdsState1.simplex(0) should be (originPoint)
    mdsState1.simplex(1) should be (point1)
    mdsState1.simplex(2) should be (point2)
    mdsState1.bestPoint should be(originPoint)
    mdsState1.getBestPoint should be(originPoint)
  }
  
  test("Test MultiDirectionalSearch dummy 2D - Advanced initialization of a MultiDirectionalSearchState") {
    val mdsState1 = MultiDirectionalSearchState(originPoint.coordinates, unit2DIntervals, evaluator, feasReg, comparator)
    mdsState1.simplex(0).coordinates should be (originPoint.coordinates)
    assert(math.abs(mdsState1.simplex(1).coordinates(0)) < 0.5)
    assert(math.abs(mdsState1.simplex(1).coordinates(1)) < 0.5)
    assert(math.abs(mdsState1.simplex(2).coordinates(0)) < 0.5)
    assert(math.abs(mdsState1.simplex(2).coordinates(1)) < 0.5)
    mdsState1.bestPoint.coordinates should be(originPoint.coordinates)
    mdsState1.getBestPoint.coordinates should be(originPoint.coordinates)
  }
  
  test("Test MultiDirectionalSearch dummy 2D - Rotation on a MultiDirectionalSearchState") {
    val mdsState1 = MultiDirectionalSearchState(Array(originPoint, point1, point2), unit2DIntervals)
    val rotation = mdsState1.getRotation(evaluator, feasReg)
    rotation(0).coordinates should be(Array(0.0, 0.0))
    rotation(1).coordinates should be(Array(0.0, -1.0))
    rotation(2).coordinates should be(Array(-1.0, -1.0))
    mdsState1.applyMultiPointTransformation(rotation, comparator)
    mdsState1.simplex(0).coordinates should be (originPoint.coordinates)
    mdsState1.simplex(1).coordinates should be (Array(0.0, -1.0))
    mdsState1.simplex(2).coordinates should be (Array(-1.0, -1.0))
    val mdsState2 = mdsState1.getNewState(rotation(1), comparator)
    mdsState2.simplex(0).coordinates should be (Array(0.0, -1.0))
    mdsState2.simplex(1).coordinates should be (originPoint.coordinates)
    mdsState2.simplex(2).coordinates should be (Array(-1.0, -1.0))
  }
  
  test("Test MultiDirectionalSearch dummy 2D - Expansion on a MultiDirectionalSearchState") {
    val mdsState1 = MultiDirectionalSearchState(Array(originPoint, point1, point2), unit2DIntervals)
    val expansion = mdsState1.getExpansion(evaluator, feasReg)
    expansion(0).coordinates should be(Array(0.0, 0.0))
    expansion(1).coordinates should be(Array(0.0, -2.0))
    expansion(2).coordinates should be(Array(-2.0, -2.0))
    mdsState1.applyMultiPointTransformation(expansion, comparator)
    mdsState1.simplex(0).coordinates should be (Array(0.0, 0.0))
    mdsState1.simplex(1).coordinates should be (Array(0.0, -2.0))
    mdsState1.simplex(2).coordinates should be (Array(-2.0, -2.0))
    val mdsState2 = mdsState1.getNewState(expansion(1), comparator)
    mdsState2.simplex(0).coordinates should be (Array(0.0, -2.0))
    mdsState2.simplex(1).coordinates should be (originPoint.coordinates)
    mdsState2.simplex(2).coordinates should be (Array(-2.0, -2.0))
  }
  
  test("Test MultiDirectionalSearch dummy 2D - Shrink on a MultiDirectionalSearchState") {
    val mdsState1 = MultiDirectionalSearchState(Array(originPoint, point1, point2), unit2DIntervals)
    val shrink = mdsState1.getShrink(evaluator, feasReg)
    shrink(0).coordinates should be(Array(0.0, 0.0))
    shrink(1).coordinates should be(Array(0.0, 0.5))
    shrink(2).coordinates should be(Array(0.5, 0.5))
    mdsState1.applyMultiPointTransformation(shrink, comparator)
    mdsState1.simplex(0).coordinates should be (Array(0.0, 0.0))
    mdsState1.simplex(1).coordinates should be (Array(0.0, 0.5))
    mdsState1.simplex(2).coordinates should be (Array(0.5, 0.5))
  }
  
  def dummySquareEval(coordinates: Array[Double]): Array[Double] = {
    def f1 = math.pow(coordinates(0), 2)
    def f2 = math.pow(coordinates(1), 2)
    Array(f1, f2)
  }
  
  def inUnitInterval(ar: Array[Double]): Boolean = {
    for (e <- ar) {
      if (e < -10.0 || e > 10.0)
        return false
    }
    true
  }
}