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
import oscar.visual.PlotDFOPareto2D
import oscar.visual.VisualFrame
import oscar.dfo.mogen.MOGENTriplet
import oscar.util.mo.ParetoFront
import oscar.util.VisualController._
import scala.util.continuations._
import oscar.dfo.mogen.algos.DirectionalDirectSearch

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class TestMOGEN extends FunSuite with ShouldMatchers {
  /*
  test("Test MOGEN dummy 2D - Only Nelder-Mead") {
    val nbCoords = 2
    val nbEvals = 2
    val nbPoints = 100
    val nbIterations = 100
    
    /** The frame used to observe Pareto front improvement */
    val f = new VisualFrame("MOGEN", 1, 2)
    /** The toolbar with the play, pause and next buttons */
    val toolBar = f.createToolBar(withVisuController = true)
    /** The visualisation pareto plot */
    val paretoPlot = PlotDFOPareto2D[Double](nbPareto = 1, objMax1 = false, objMax2 = false)
    f.add(paretoPlot)
    f.pack()
    
    
    MOGEN.onIterateSelected = (triplet: MOGENTriplet[_]) => {
      println("onIterateSelected")
      withController {
        println("inside withController")
        paretoPlot.highLightIterate(triplet.asInstanceOf[MOGENTriplet[Double]])
        pause()
      }
    }
    MOGEN.onArchiveChanged = (archive: ParetoFront[_]) => {
      withController {
        paretoPlot.update(archive.asInstanceOf[ParetoFront[Double]])
        pause()
      }
    }
    
    withController{
      println("I begin")
      pause()
    }
    
    val mogen = MOGEN(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double](), visu=true)
    mogen.initFeasibleReagion(List(inUnitInterval))
    mogen.initArchive(nbPoints, Array.fill(nbCoords)((0.0, 1.0)), List((NelderMead, 1.0)))
    println(mogen.archive.toSet.size)
    val paretoEstimation = mogen.optimizeMOO(nbIterations)
    for (mooPoint <- paretoEstimation) {
      println(mooPoint)
    }
    paretoEstimation.size > 1 should be(true)
  }
  
  test("Test MOGEN dummy 2D - Only Directional Direct-Search") {
    val nbCoords = 2
    val nbEvals = 2
    val nbPoints = 100
    val nbIterations = 100
    
    /** The frame used to observe Pareto front improvement */
    val f = new VisualFrame("MOGEN", 1, 2)
    /** The toolbar with the play, pause and next buttons */
    val toolBar = f.createToolBar(withVisuController = true)
    /** The visualisation pareto plot */
    val paretoPlot = PlotDFOPareto2D[Double](nbPareto = 1, objMax1 = false, objMax2 = false)
    f.add(paretoPlot)
    f.pack()
    
    
    MOGEN.onIterateSelected = (triplet: MOGENTriplet[_]) => {
      println("onIterateSelected")
      withController {
        println("inside withController")
        paretoPlot.highLightIterate(triplet.asInstanceOf[MOGENTriplet[Double]])
        pause()
      }
    }
    MOGEN.onArchiveChanged = (archive: ParetoFront[_]) => {
      withController {
        paretoPlot.update(archive.asInstanceOf[ParetoFront[Double]])
        pause()
      }
    }
    
    withController{
      println("I begin")
      pause()
    }
    
    val mogen = MOGEN(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double](), visu=true)
    mogen.initFeasibleReagion(List(inUnitInterval))
    mogen.initArchive(nbPoints, Array.fill(nbCoords)((0.3, 0.4)), List((DirectionalDirectSearch, 1.0)))
    println(mogen.archive.toSet.size)
    val paretoEstimation = mogen.optimizeMOO(nbIterations)
    for (mooPoint <- paretoEstimation) {
      println(mooPoint)
    }
    paretoEstimation.size > 1 should be(true)
  }
  
  
  test("Test MOGEN dummy 3D") {
    val nbCoords = 3
    val nbEvals = 3
    val nbPoints = 100
    val nbIterations = 100
    val mogen = MOGEN(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double]())
    mogen.initFeasibleReagion(List(inUnitInterval))
    mogen.initArchive(nbPoints, Array.fill(nbCoords)((0.0, 1.0)), List((NelderMead, 1.0)))
    val paretoEstimation = mogen.optimizeMOO(nbIterations)
    for (mooPoint <- paretoEstimation) {
      println(mooPoint)
    }
    paretoEstimation.size should be(1)
  }
  */

  def zdt1(coordinates: Array[Double]): Array[Double] = {
    def g = 1 + (9 / (coordinates.length - 1)) * (coordinates.drop(1).sum)
    def f1 = coordinates(0)
    def f2 = g * (1.0 - math.sqrt(f1/g))
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
