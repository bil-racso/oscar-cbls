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
import oscar.util.mo.MOOPoint
import oscar.util.mo.MaxMOOComparator
import oscar.util.mo.LinearList
import oscar.dfo.mogen.MOGEN
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MinMOOComparator
import oscar.dfo.mogen.algos.NelderMead

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class TestMOGEN extends FunSuite with ShouldMatchers {
  test("Test MOGEN dummy 2D") {
    val nbCoord = 2
    val nbEvals = 2
    val nbPoints = 100
    val nbIterations = 10
    val mogen = MOGEN(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double]())
    mogen.initFeasibleReagion(List(inUnitInterval))
    mogen.initArchive(nbPoints, Array.fill(nbCoord)((0.0, 1.0)), List((NelderMead, 1.0)))
    val paretoEstimation = mogen.optimizeMOO(nbIterations)
    for (mooPoint <- paretoEstimation) {
      println(mooPoint)
    }
    paretoEstimation.size > 1 should be(true)
  }
  
  test("Test MOGEN dummy 3D") {
    val nbCoord = 3
    val nbEvals = 3
    val nbPoints = 100
    val nbIterations = 100
    val mogen = MOGEN(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double]())
    mogen.initFeasibleReagion(List(inUnitInterval))
    mogen.initArchive(nbPoints, Array.fill(nbCoord)((0.0, 1.0)), List((NelderMead, 1.0)))
    val paretoEstimation = mogen.optimizeMOO(nbIterations)
    for (mooPoint <- paretoEstimation) {
      println(mooPoint)
    }
    paretoEstimation.size should be(1)
  }

  def zdt1(coordinates: Array[Double]): Array[Double] = {
    def g = 1 + 9 * (coordinates.drop(1).sum / (coordinates.length - 1))
    def f1 = coordinates(0)
    def f2 = g * (1 - math.sqrt(f1/g))
    Array(f1, f2)
  }
  
  def inUnitInterval(ar: Array[Double]): Boolean = {
    for (e <- ar) {
      if (e < 0 || e > 1)
        return false
    }
    true
  }
}