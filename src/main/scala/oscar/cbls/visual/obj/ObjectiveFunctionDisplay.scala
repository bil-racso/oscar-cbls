package oscar.cbls.visual.obj

import java.awt.Color

import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import oscar.cbls._
import oscar.cbls.util.StopWatch
import oscar.visual.plot.Plot

class ObjectiveFunctionDisplay(title: String, cap:Long = Long.MaxValue)
  extends LongPlot(title,"Time","Objective function value", 2) with StopWatch {

  xDom = new org.jfree.data.Range(0,1)
  yDom = new org.jfree.data.Range(0,100)

  private val startinAt = getWatch
  private var best = Long.MaxValue
  panel.setMouseWheelEnabled(true)
  panel.setMouseZoomable(true)
  panel.setHorizontalAxisTrace(true)
  panel.setVerticalAxisTrace(true)

  val r1 = new XYLineAndShapeRenderer
  r1.setSeriesPaint(1, Color.GREEN)
  r1.setSeriesPaint(0, Color.RED)
  r1.setSeriesShapesVisible(0, false)
  r1.setSeriesShapesVisible(1, false)

  plot.setRenderer(r1)

  this.setDoubleBuffered(true)
  panel.setDoubleBuffered(true)

  def createChart =
    ChartFactory.createXYLineChart(
      null,
      null,
      null,
      xyDataset,PlotOrientation.VERTICAL,
      false,
      false,
      false)

  def drawFunction(value: Long) ={

    val at = (getWatch - startinAt).toDouble/1000

    if(yDom.getUpperBound < value.toDouble)
      yDom = new org.jfree.data.Range(0,upper(value,cap))
    if(xDom.getUpperBound < at)
      xDom = new org.jfree.data.Range(0,upper(at))

    addPoint(at,value.toDouble,0)

    if(value <= best) {
      best = value
    }
    addPoint(at, best.toDouble, 1)

  }

  private def upper(value: Double, cap:Long = Long.MaxValue): Long ={
    //TODO why not just make an integer or long division?
    var resExp:Long = 10
    while(value/resExp > 1) {
      resExp = resExp * 10
    }
    resExp = resExp/10
    var res = resExp
    while(value/res > 1){
      res += resExp
    }
    res min cap
  }

}

object ObjectiveFunctionDisplay{
  def apply(title: String, cap:Long = Long.MaxValue): ObjectiveFunctionDisplay = new ObjectiveFunctionDisplay(title,cap)
}
