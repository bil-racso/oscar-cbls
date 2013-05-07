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

package oscar.dfo.mogen.algos.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.util.mo.MOOPoint
import oscar.util.mo.MaxMOOComparator
import oscar.util.mo.LinearList
import oscar.dfo.mogen.MOGEN
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MinMOOComparator
import oscar.dfo.mogen.algos.NelderMead
import oscar.visual.PlotDFOPareto2D
import oscar.visual.VisualFrame
import oscar.dfo.mogen.MOGENTriplet
import oscar.util.mo.ParetoFront
import oscar.util.VisualController._
import scala.util.continuations._
import oscar.dfo.mogen.algos.DirectionalDirectSearch
import oscar.dfo.mogen.algos.states.NelderMeadState
import oscar.util.mo.FeasibleRegion
import oscar.util.mo.MOOComparator

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class TestMOGENNelderMead extends FunSuite with ShouldMatchers {
  val originPoint = MOOPoint(Array(0.0, 0.0), Array(0.0, 0.0))
  val point1 = MOOPoint(Array(0.0, 1.0), Array(0.0, 1.0))
  val point2 = MOOPoint(Array(1.0, 0.0), Array(1.0, 0.0))
  val unit2DIntervals = Array((0.0, 1.0), (0.0, 1.0))
  val evaluator = MOEvaluator(dummySquareEval, Array(Double.MaxValue, Double.MaxValue))
  val feasReg = FeasibleRegion(List(inUnitInterval))
  val comparator = MinMOOComparator[Double]()
  
  test("Test Nelder-Mead dummy 2D - Basic initialization of a NelderMeadState") {
    val nmState1 = NelderMeadState(Array(originPoint, point1, point2), unit2DIntervals)
    nmState1.simplex(0) should be (originPoint)
    nmState1.simplex(1) should be (point1)
    nmState1.simplex(2) should be (point2)
    nmState1.bestPoint should be(originPoint)
    nmState1.getBestPoint should be(originPoint)
  }
  
  test("Test Nelder-Mead dummy 2D - Advanced initialization of a NelderMeadState") {
    val nmState1 = NelderMeadState(originPoint.coordinates, unit2DIntervals, evaluator, feasReg, comparator)
    nmState1.simplex(0).coordinates should be (originPoint.coordinates)
    assert(math.abs(nmState1.simplex(1).coordinates(0)) < 0.5)
    assert(math.abs(nmState1.simplex(1).coordinates(1)) < 0.5)
    assert(math.abs(nmState1.simplex(2).coordinates(0)) < 0.5)
    assert(math.abs(nmState1.simplex(2).coordinates(1)) < 0.5)
    nmState1.bestPoint.coordinates should be(originPoint.coordinates)
    nmState1.getBestPoint.coordinates should be(originPoint.coordinates)
  }
  
  test("Test Nelder-Mead dummy 2D - Reflection on a NelderMeadState") {
    val nmState1 = NelderMeadState(Array(originPoint, point1, point2), unit2DIntervals)
    val centroid = nmState1.getCentroid
    val reflection = nmState1.getReflection(evaluator, feasReg, centroid)
    centroid should be(Array(0.0, 0.5))
    reflection should be(Array(-1.0, 1.0))
    nmState1.applySinglePointTransformation(reflection, comparator)
  }
  
  test("Test Nelder-Mead dummy 2D - Expansion on a NelderMeadState") {
    val nmState1 = NelderMeadState(Array(originPoint, point1, point2), unit2DIntervals)
    val centroid = nmState1.getCentroid
    val expansion = nmState1.getExpansion(evaluator, feasReg, centroid)
  }
  
  test("Test Nelder-Mead dummy 2D - Inside Contraction on a NelderMeadState") {
    val nmState1 = NelderMeadState(Array(originPoint, point1, point2), unit2DIntervals)
    val centroid = nmState1.getCentroid
    val insideContraction = nmState1.getInsideContraction(evaluator, feasReg, centroid)
  }
  
  test("Test Nelder-Mead dummy 2D - Outside Contraction on a NelderMeadState"){
    val nmState1 = NelderMeadState(Array(originPoint, point1, point2), unit2DIntervals)
    val centroid = nmState1.getCentroid
    val outsideContraction = nmState1.getOutsideContraction(evaluator, feasReg, centroid)
  }
  
  test("Test Nelder-Mead dummy 2D - Shrink on a NelderMeadState"){
    val nmState1 = NelderMeadState(Array(originPoint, point1, point2), unit2DIntervals)
    val shrink = nmState1.applyShrink(comparator, evaluator, feasReg)
  }
  
  
  def dummySquareEval(coordinates: Array[Double]): Array[Double] = {
    def f1 = math.pow(coordinates(0), 2)
    def f2 = math.pow(coordinates(1), 2)
    Array(f1, f2)
  }
  
  def inUnitInterval(ar: Array[Double]): Boolean = {
    for (e <- ar) {
      if (e < 0.0 || e > 1.0)
        return false
    }
    true
  }
}