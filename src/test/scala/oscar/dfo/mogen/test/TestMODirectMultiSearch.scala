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
import oscar.visual.PlotDFOPareto2D
import oscar.visual.VisualFrame
import oscar.dfo.mogen.MOGENTriplet
import oscar.util.mo.ParetoFront
import oscar.util.VisualController._
import scala.util.continuations._
import oscar.dfo.mogen.algos.DirectionalDirectSearch
import oscar.dfo.mogen.modms.MODirectMultiSearch
import oscar.dfo.mogen.modms.MODMSElement
import oscar.dfo.mogen.algos.MultiDirectionalSearch

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class TestMODirectMultiSearch extends FunSuite with ShouldMatchers {
  
  test("Test MODirectionalDirectSearch - ZDT1") {
    val nbCoords = 2
    val nbEvals = 2
    val nbPoints = 100
    val nbIterations = 8000
    
    /** The frame used to observe Pareto front improvement */
    val f = new VisualFrame("MOGEN", 1, 2)
    /** The visualisation pareto plot */
    val paretoPlot = PlotDFOPareto2D[Double](nbPareto = 1, objMax1 = false, objMax2 = false)
    f.add(paretoPlot)
    f.pack()
    
    MODirectMultiSearch.onIterateSelected{
      (element: MODMSElement[_]) => {
        paretoPlot.highLightIterate(element.asInstanceOf[MODMSElement[Double]])
        Thread.sleep(50)
      }
    }
    MODirectMultiSearch.onArchiveChanged {
      (archive: ParetoFront[_]) => {
        paretoPlot.update(archive.asInstanceOf[ParetoFront[Double]])
        Thread.sleep(50)
      }
    }
    
    val modms = MODirectMultiSearch(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double]())
    modms.initFeasibleReagion(List(inUnitInterval))
    modms.initArchive(nbPoints, Array.fill(nbCoords)((0.0, 1.0)), 1.0)
    println("Initial size of the archive: " + modms.archive.toSet.size)
    val paretoEstimation = modms.optimizeMOO(nbIterations)
    for (mooPoint <- paretoEstimation) {
      println(mooPoint)
    }
    paretoEstimation.size > 1 should be(true)
  }
  
  test("Test MODirectionalDirectSearch vs MOGEN with Nelder-Mead - ZDT1") {
    val nbCoords = 4
    val nbEvals = 2
    val nbPoints = 100
    val nbIterations = 8000
    
    /** The frame used to observe Pareto front improvement */
    val f = new VisualFrame("MOGEN vs MODMS", 2, 2)
    val inf1 = f.createFrame("MOGEN")
    val inf2 = f.createFrame("MODMS")
    /** The visualisation pareto plot for MOGEN */
    val paretoPlotMOGEN = PlotDFOPareto2D[Double](title="Archive for MOGEN", nbPareto = 1, objMax1 = false, objMax2 = false)
    /** The visualisation pareto plot for MODMS */
    val paretoPlotMODMS = PlotDFOPareto2D[Double](title="Archive for MODMS", nbPareto = 1, objMax1 = false, objMax2 = false)
    inf1.add(paretoPlotMOGEN)
    inf2.add(paretoPlotMODMS)
    inf1.pack()
    inf2.pack()
    
    MODirectMultiSearch.onIterateSelected{
      (element: MODMSElement[_]) => {
        paretoPlotMODMS.highLightIterate(element.asInstanceOf[MODMSElement[Double]])
      }
    }
    MODirectMultiSearch.onArchiveChanged {
      (archive: ParetoFront[_]) => {
        paretoPlotMODMS.update(archive.asInstanceOf[ParetoFront[Double]])
      }
    }
    
    MOGEN.onIterateSelected{
      (element: MOGENTriplet[_]) => {
        paretoPlotMOGEN.highLightIterate(element.asInstanceOf[MOGENTriplet[Double]])
      }
    }
    MOGEN.onArchiveChanged {
      (archive: ParetoFront[_]) => {
        paretoPlotMOGEN.update(archive.asInstanceOf[ParetoFront[Double]])
      }
    }
        
    val mogen = MOGEN(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double](), visu=true)
    mogen.initFeasibleReagion(List(inUnitInterval))
    mogen.initArchive(nbPoints, Array.fill(nbCoords)((0.0, 1.0)), List((DirectionalDirectSearch, 1.0)))//List((DirectionalDirectSearch, 1.0), (NelderMead, 1.0), (MultiDirectionalSearch, 1.0)))
    
    val modms = MODirectMultiSearch(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double]())
    modms.initFeasibleReagion(List(inUnitInterval))
    for(point <- mogen.archive.toSet) {
      modms.archive.insert(MODMSElement(point, 1.0), MinMOOComparator[Double]())
    }
    
    for (i <- 1 to nbIterations) {
      mogen.performIteration(i)
      modms.performIteration(i)
      Thread.sleep(200)
    }
    
    println("Initial size of the archive: " + modms.archive.toSet.size)
    modms.archive.toSet.size > 1 should be(true)
  }
  
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