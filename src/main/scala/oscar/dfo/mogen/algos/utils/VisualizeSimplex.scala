package oscar.dfo.mogen.algos.utils

import scala.util.continuations._
import oscar.util.VisualController.pause
import oscar.util.mo.MOOPoint
import oscar.visual.VisualSimplex2D

trait VisualizeSimplex[E] extends Simplex[E] {
  var inputSimplexPlot: VisualSimplex2D 
  var outputSimplexPlot: VisualSimplex2D
  
  override def onInit(): Unit@suspendable = {
    inputSimplexPlot.update(simplex.asInstanceOf[Array[MOOPoint[Double]]])
    outputSimplexPlot.update(simplex.asInstanceOf[Array[MOOPoint[Double]]])
    pause()
  }
  
  override def onReflexion(reflectedPoint: MOOPoint[E]): Unit@suspendable = {
    inputSimplexPlot.showReflexion(simplex.asInstanceOf[Array[MOOPoint[Double]]], reflectedPoint.asInstanceOf[MOOPoint[Double]])
    outputSimplexPlot.showReflexion(simplex.asInstanceOf[Array[MOOPoint[Double]]], reflectedPoint.asInstanceOf[MOOPoint[Double]])
    pause()
  }
  
  override def onExpansion(expandedPoint: MOOPoint[E]): Unit@suspendable = {
    inputSimplexPlot.showExpansion(simplex.asInstanceOf[Array[MOOPoint[Double]]], expandedPoint.asInstanceOf[MOOPoint[Double]])
    outputSimplexPlot.showExpansion(simplex.asInstanceOf[Array[MOOPoint[Double]]], expandedPoint.asInstanceOf[MOOPoint[Double]])
    pause()
  }
  
  override def onInsideContraction(contractedPoint: MOOPoint[E]): Unit@suspendable = {
    inputSimplexPlot.showInsideContraction(simplex.asInstanceOf[Array[MOOPoint[Double]]], contractedPoint.asInstanceOf[MOOPoint[Double]])
    outputSimplexPlot.showInsideContraction(simplex.asInstanceOf[Array[MOOPoint[Double]]], contractedPoint.asInstanceOf[MOOPoint[Double]])
    pause()
  }
  
  override def onOutsideContraction(contractedPoint: MOOPoint[E]): Unit@suspendable = {
    inputSimplexPlot.showOutsideContraction(simplex.asInstanceOf[Array[MOOPoint[Double]]], contractedPoint.asInstanceOf[MOOPoint[Double]])
    outputSimplexPlot.showOutsideContraction(simplex.asInstanceOf[Array[MOOPoint[Double]]], contractedPoint.asInstanceOf[MOOPoint[Double]])
    pause()
  }
  
  override def onShrink(expandedPoint: MOOPoint[E]): Unit@suspendable = {}
}