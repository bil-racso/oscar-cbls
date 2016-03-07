package oscar.examples.cbls.routing.visual.FunctionGraphic

/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

import java.awt.BorderLayout
import java.awt.event.{AdjustmentListener, AdjustmentEvent}
import javax.swing.{SwingConstants, JScrollBar}

import oscar.cbls.search.algo.LazyQuicksort


/**
  * This file contains all the add-on modules that you could add to your FunctionGraphicContainer object.
  * This way you can build any FunctionGraphicContainer object you want just by adding the desired trait on the construction.
  *
  * @author fabian.germeau@student.vinci.be
  */



//TODO: Javadoc
trait Zoom extends ObjFunctionGraphicContainer{
  val zoomScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,9)
  zoomScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      adjustScrollBar()
      graphic.minXValueDisplayed = Math.max(rightLeftScrollBar.getValue,graphic.minXValue())
      graphic.maxXValueDisplayed = Math.min(graphic.minXValueDisplayed+rightLeftScrollBar.getVisibleAmount,graphic.maxXValue())
      graphic.minYValueDisplayed = Math.max(upDownScrollBar.getValue,graphic.minYValue())
      graphic.maxYValueDisplayed = Math.min(graphic.minYValueDisplayed+upDownScrollBar.getVisibleAmount,graphic.maxYValue())
      graphic.drawGlobalCurve()
    }
  })
  add(zoomScrollBar, BorderLayout.EAST)

  val rightLeftScrollBar = new JScrollBar(SwingConstants.HORIZONTAL,0,1,0,100)
  rightLeftScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.minXValueDisplayed = rightLeftScrollBar.getValue
      graphic.maxXValueDisplayed = graphic.minXValueDisplayed + rightLeftScrollBar.getVisibleAmount
      graphic.drawGlobalCurve()
    }
  })
  add(rightLeftScrollBar, BorderLayout.SOUTH)

  val upDownScrollBar = new JScrollBar(SwingConstants.VERTICAL,0,1,0,100)
  upDownScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      graphic.minYValueDisplayed = upDownScrollBar.getMaximum - upDownScrollBar.getVisibleAmount - upDownScrollBar.getValue
      graphic.maxYValueDisplayed = graphic.minYValueDisplayed + upDownScrollBar.getVisibleAmount
      graphic.drawGlobalCurve()
    }
  })
  add(upDownScrollBar, BorderLayout.WEST)

  //TODO: Javadoc
  def adjustScrollBar(): Unit ={
    rightLeftScrollBar.setVisibleAmount((graphic.maxXValue()/(2*getLogZoom(zoomScrollBar.getValue))).toInt)
    rightLeftScrollBar.setBlockIncrement(rightLeftScrollBar.getVisibleAmount)
    rightLeftScrollBar.setUnitIncrement(rightLeftScrollBar.getVisibleAmount)
    rightLeftScrollBar.setValue(0)
    upDownScrollBar.setVisibleAmount((graphic.maxYValue()/(2*getLogZoom(zoomScrollBar.getValue))).toInt)
    upDownScrollBar.setBlockIncrement(upDownScrollBar.getVisibleAmount)
    upDownScrollBar.setUnitIncrement(upDownScrollBar.getVisibleAmount)
    upDownScrollBar.setValue(0)
  }

  //TODO: Javadoc
  override def drawGlobalCurve(): Unit ={
    zoomScrollBar.setValue(0)
    rightLeftScrollBar.setMaximum(graphic.maxXValue().toInt+1)
    upDownScrollBar.setMaximum(graphic.maxYValue()+1)
    super.drawGlobalCurve()
  }


  def getLogZoom(i:Double): Double ={
    if(i == 0)0.5
    else Math.max(1.0,100*Math.pow(Math.log(i), 2) / Math.pow(Math.log(100), 2))
  }
}

trait AdjustMaxValue extends ObjFunctionGraphicContainer{
  var sortedYValues:LazyQuicksort = null

  val adjustMaxValueScrollBar = new JScrollBar(SwingConstants.VERTICAL,1,1,1,101)
  adjustMaxValueScrollBar.addAdjustmentListener(new AdjustmentListener {
    override def adjustmentValueChanged(e: AdjustmentEvent): Unit = {
      sortedYValues.sortUntil(Math.max(1,adjustMaxValueScrollBar.getValue/100 * sortedYValues.size))
      val iteratorXValues = sortedYValues.iterator
      for(i <- 0 until Math.max(1, (adjustMaxValueScrollBar.getValue.toDouble / 100 * sortedYValues.size).toInt)){
        val temp = iteratorXValues.next()
        if(i == (adjustMaxValueScrollBar.getValue.toDouble/100 * sortedYValues.size).toInt-1) {
          graphic.maxYValueDisplayed = temp
        }
      }
      graphic.drawGlobalCurve()
    }
  })
  add(adjustMaxValueScrollBar, BorderLayout.EAST)

  override def drawGlobalCurve(): Unit ={
    sortedYValues = new LazyQuicksort(graphic.yValues.toArray,+_)
    super.drawGlobalCurve()
  }
}