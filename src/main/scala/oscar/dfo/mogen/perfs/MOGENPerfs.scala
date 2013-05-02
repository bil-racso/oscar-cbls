package oscar.dfo.mogen.perfs

import oscar.visual.VisualFrame
import oscar.visual.PlotDFOPareto2D
import oscar.util.VisualController._
import scala.util.continuations._
import oscar.dfo.mogen.MOGEN
import oscar.dfo.mogen.MOGENTriplet
import oscar.util.mo.ParetoFront
import oscar.util.mo.MOEvaluator
import oscar.util.mo.MinMOOComparator
import oscar.dfo.mogen.algos.NelderMead

object MOGENPerfs {
  def main(args: Array[String]): Unit = {
  val nbCoords = 2
    val nbEvals = 2
    val nbPoints = 100
    val nbIterations = 800
    
    /** The frame used to observe Pareto front improvement */
    val f = new VisualFrame("MOGEN", 4, 4)
    /** The toolbar with the play, pause and next buttons */
    val toolBar = f.createToolBar(withVisuController = true)
    /** The visualisation pareto plot */
    val paretoPlot = PlotDFOPareto2D[Double](nbPareto = 1, objMax1 = false, objMax2 = false)
    f.add(paretoPlot)
    f.pack()
    
    //for (i <- 0 until 43) println(i)
    
    withController {
    /*
      MOGEN.onIterateSelected{
        (triplet: MOGENTriplet[_]) => {
          paretoPlot.highLightIterate(triplet.asInstanceOf[MOGENTriplet[Double]])
          pause()
        }
      }
      
      MOGEN.onArchiveChanged {
        (archive: ParetoFront[_]) => {
          paretoPlot.update(archive.asInstanceOf[ParetoFront[Double]])
          pause()
        }
      }
    */
      val mogen = MOGEN(MOEvaluator(zdt1, Array.fill(nbEvals)(Double.MaxValue)), MinMOOComparator[Double](), visu=true)
      mogen.initFeasibleReagion(List(inUnitInterval))
      mogen.initArchive(nbPoints, Array.fill(nbCoords)((0.0, 1.0)), List((NelderMead, 1.0)))
      //paretoPlot.update(mogen.archive)
      println(mogen.archive.toSet.size)
      val paretoEstimation = mogen.optimizeMOO(nbIterations)
      for (mooPoint <- paretoEstimation) {
        println(mooPoint)
      }
    }
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